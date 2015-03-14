{-# LANGUAGE BangPatterns #-}
module Tracy.Main where

import Control.Lens
import Control.Concurrent
import Data.IORef
import Data.Time.Clock
import System.Exit
import System.Random.MWC
import qualified Data.Map as M

import Tracy.Types
import Tracy.Objects.Mesh (loadMeshes)

defaultRenderConfig :: RenderConfig
defaultRenderConfig =
    RenderConfig { _sampleRoot = 4
                 , _forceShadows = Nothing
                 , _samplesPerChunk = 1
                 , _rowsPerChunk = Height 100
                 , _renderMode = DepthFirst
                 }

render :: String
       -> RenderConfig
       -> SceneDesc
       -> (Frame, Frame)
       -> Int
       -> (Chan JobRequest -> Chan (String, JobResponse) -> IO ())
       -> Chan InfoEvent
       -> Chan DataEvent
       -> IO ()
render sceneName renderCfg s frameRange numNodes renderManager iChan dChan = do
  let w = s^.sceneDescWorld
      height = w^.wdViewPlane.vpVres
      allRows = [Row 0..Row $ fromEnum (height-1)]
      allSampleIndices = [0..fromEnum (((renderCfg^.sampleRoot) ** 2) - 1)]
      Height chunkHeight = renderCfg^.rowsPerChunk
      (startFrame, endFrame) = frameRange

      ranges _ [] = []
      ranges n rs = (f, l) : ranges n rest
          where
            f = head current
            l = head $ reverse current
            current = take n rs
            rest = drop n rs

      rowRanges = ranges chunkHeight allRows
      sampleRanges = ranges (renderCfg^.samplesPerChunk) allSampleIndices

      requests = if renderCfg^.renderMode == BreadthFirst
                 then [ RenderRequest rowRange sampleRange
                      | sampleRange <- sampleRanges
                      , rowRange <- rowRanges
                      ]
                 else [ RenderRequest rowRange sampleRange
                      | rowRange <- rowRanges
                      , sampleRange <- sampleRanges
                      ]

  writeChan iChan $ ISceneName sceneName
  writeChan iChan $ IFrameRange frameRange
  writeChan dChan $ DSceneName sceneName

  writeChan iChan $ ISampleRoot $ renderCfg^.sampleRoot
  writeChan iChan $ INumObjects $ Count $ w^.wdObjects.to length
  writeChan iChan $ IShadows $ w^.wdWorldShadows
  writeChan iChan $ IImageSize (Width $ fromEnum $ w^.wdViewPlane.vpHres)
                               (Height $ fromEnum $ w^.wdViewPlane.vpVres)

  writeChan dChan $ DSampleRoot $ renderCfg^.sampleRoot
  writeChan dChan $ DImageSize (Width $ fromEnum $ w^.wdViewPlane.vpHres)
                               (Height $ fromEnum $ w^.wdViewPlane.vpVres)
  writeChan dChan $ DRowRanges rowRanges

  writeChan iChan ILoadingMeshes

  -- Preload meshes
  !mg <- loadMeshes s

  writeChan iChan $ ILoadedMeshes $ Count $ M.size mg

  reqChan <- newChan
  respChan <- newChan

  -- Start the renderer thread
  _ <- forkIO (renderManager reqChan respChan)

  -- Set up the RNG state
  gen <- createSystemRandom
  rngSeed <- save gen
  let rngSeedV = fromSeed rngSeed

  startTime <- newIORef Nothing

  -- Wait for the responses
  let collector curFrame numFinished = do
        resp <- readChan respChan
        let node = fst resp
        case snd resp of
            JobError msg -> do
                putStrLn $ "Yikes! Error in render thread, node " ++ node ++ ": " ++ msg
                exitSuccess
            JobAck -> collector curFrame numFinished
            SetFrameAck -> do
                collector curFrame numFinished
            ChunkFinished rowRange sc rs -> do
                -- If we get here and startTime is Nothing, that's a
                -- bug; how could we have finished a chunk if we hadn't
                -- gotten at least one scene ack?
                Just t1 <- readIORef startTime
                t <- getCurrentTime
                let remainingTime = toEnum $ timePerCompletedChunk * chunksRemaining
                    chunksFinished = numFinished + 1 + ((length requests) * completedFrames)
                    chunksRemaining = (length requests) * framesLeft + (length requests - (numFinished + 1))
                    timePerCompletedChunk = (fromEnum $ diffUTCTime t t1) `div` chunksFinished
                    framesLeft = fe - fc
                    completedFrames = fc - 1
                    Frame fc = curFrame
                    Frame fe = endFrame

                writeChan iChan $ IChunkFinished curFrame (Count $ numFinished + 1) (Count $ length requests) remainingTime
                writeChan dChan $ DChunkFinished rowRange sc rs

                case numFinished + 1 == length requests of
                    False -> collector curFrame $ numFinished + 1
                    True -> do
                        writeChan reqChan FrameFinished
                        writeChan dChan $ DFinished curFrame
                        writeChan iChan $ IFinished curFrame
            _ -> do
                putStrLn $ "Yikes! Unexpected response in rendering loop, node " ++ node
                exitSuccess

      doFrames curFrame = do
        -- Set the frame on all nodes
        writeChan reqChan $ SetFrame curFrame
        writeChan dChan $ DStarted curFrame
        mapM_ (writeChan reqChan) requests
        collector curFrame 0
        if curFrame /= endFrame
           then doFrames (curFrame + Frame 1)
           else do
               writeChan reqChan RenderFinished
               writeChan reqChan Shutdown

      waitForReady ready = do
        resp <- readChan respChan
        let node = fst resp
        case snd resp of
            JobError e -> do
                putStrLn $ "Yikes! Error waiting for scene ack, node " ++ node ++ ": " ++ e
                exitSuccess
            SetSceneAck -> do
                -- Since scene-setting takes a while (all sample data
                -- and indices are generated and meshes are traversed),
                -- we wait for all nodes to finish with this setup
                -- process. Only then, once all nodes have delivered an
                -- acknowledgement (SetSceneAck), do we begin to send
                -- out any rendering requests. We start the rendering
                -- timer once these requests start to go out. As each
                -- rendering request is finished, the next request is
                -- sent out (as deemed appropriate by the rendering
                -- manager).
                writeChan iChan $ INodeReady node
                case ready + 1 == numNodes of
                    False -> waitForReady $ ready + 1
                    True -> do
                      t1 <- getCurrentTime
                      writeChan iChan IStarted
                      writeChan iChan $ IStartTime t1
                      return t1
            JobAck -> waitForReady ready
            _ -> do
                putStrLn $ "Yikes! Unexpected response in scene ack loop, node " ++ node
                exitSuccess

  -- Set the scene
  writeChan iChan ISettingScene
  writeChan reqChan $ SetScene renderCfg s mg rngSeedV rowRanges

  -- Wait for all nodes to finish setting up, then start the
  -- request/response loop
  startTimeVal <- waitForReady 0
  writeIORef startTime $ Just startTimeVal

  doFrames startFrame

  Just t1 <- readIORef startTime
  t2 <- getCurrentTime

  writeChan iChan $ IFinishTime t2
  writeChan iChan $ ITotalTime (diffUTCTime t2 t1)

  writeChan dChan DShutdown
  writeChan iChan IShutdown
