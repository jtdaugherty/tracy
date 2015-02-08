{-# LANGUAGE BangPatterns #-}
module Tracy.Main where

import Control.Lens
import Control.Concurrent
import Control.Monad (when)
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
                 , _rowsPerChunk = 100
                 , _renderMode = DepthFirst
                 }

render :: String
       -> RenderConfig
       -> SceneDesc
       -> Frame
       -> Int
       -> (Chan JobRequest -> Chan (String, JobResponse) -> IO ())
       -> Chan InfoEvent
       -> Chan DataEvent
       -> IO ()
render sceneName renderCfg s frameNum numNodes renderManager iChan dChan = do
  let w = s^.sceneDescWorld
      allRows = [Row 0..Row $ fromEnum (w^.wdViewPlane.vpVres-1)]
      allSampleIndices = [0..fromEnum (((renderCfg^.sampleRoot) ** 2) - 1)]

      ranges _ [] = []
      ranges n rs = (f, l) : ranges n rest
          where
            f = head current
            l = head $ reverse current
            current = take n rs
            rest = drop n rs

      rowRanges = ranges (renderCfg^.rowsPerChunk) allRows
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
  writeChan iChan $ IFrameNum frameNum

  writeChan dChan $ DSceneName sceneName
  writeChan dChan $ DFrameNum frameNum

  writeChan iChan $ ISampleRoot $ renderCfg^.sampleRoot
  writeChan iChan $ IAccelScheme $ s^.sceneDescAccelScheme
  writeChan iChan $ INumObjects $ w^.wdObjects.to length
  writeChan iChan $ IShadows $ w^.wdWorldShadows
  writeChan iChan $ IImageSize (fromEnum $ w^.wdViewPlane.vpHres)
                               (fromEnum $ w^.wdViewPlane.vpVres)

  writeChan dChan $ DSampleRoot $ renderCfg^.sampleRoot
  writeChan dChan $ DImageSize (fromEnum $ w^.wdViewPlane.vpHres)
                               (fromEnum $ w^.wdViewPlane.vpVres)
  writeChan dChan $ DRowRanges rowRanges

  writeChan iChan ILoadingMeshes

  -- Preload meshes
  !mg <- loadMeshes s

  writeChan iChan $ ILoadedMeshes $ M.size mg

  reqChan <- newChan
  respChan <- newChan

  -- Start the renderer thread
  _ <- forkIO (renderManager reqChan respChan)

  -- Set up the RNG state
  gen <- createSystemRandom
  rngSeed <- save gen
  let rngSeedV = fromSeed rngSeed

  -- Set the scene
  writeChan iChan ISettingScene
  writeChan reqChan $ SetScene renderCfg s mg frameNum rngSeedV rowRanges

  startTime <- newIORef Nothing
  numReadyNodes <- newIORef 0

  -- Wait for the responses
  let collector numFinished = do
        resp <- readChan respChan
        let node = fst resp
        case snd resp of
            JobError msg -> do
                putStrLn $ "Yikes! Error in render thread, node " ++ node ++ ": " ++ msg
                exitSuccess
            JobAck -> collector numFinished
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
                modifyIORef numReadyNodes (+ 1)
                ready <- readIORef numReadyNodes

                when (ready == numNodes) $ do
                      -- Send the rendering requests
                      mapM_ (writeChan reqChan) requests

                      t1 <- getCurrentTime
                      writeChan iChan IStarted
                      writeChan iChan $ IStartTime t1
                      writeChan dChan DStarted
                      writeIORef startTime $ Just t1

                collector numFinished
            ChunkFinished rowRange rs -> do
                -- If we get here and startTime is Nothing, that's a
                -- bug; how could we have finished a chunk if we hadn't
                -- gotten at least one scene ack?
                Just t1 <- readIORef startTime
                t <- getCurrentTime
                let remainingTime = toEnum $ ((fromEnum $ diffUTCTime t t1) `div` (numFinished + 1)) *
                                             (length requests - (numFinished + 1))

                writeChan iChan $ IChunkFinished (numFinished + 1) (length requests) remainingTime
                writeChan dChan $ DChunkFinished rowRange rs

                if numFinished + 1 == length requests then
                   writeChan reqChan RenderFinished >> writeChan reqChan Shutdown else
                   collector $ numFinished + 1

  collector 0

  Just t1 <- readIORef startTime
  t2 <- getCurrentTime

  writeChan dChan DFinished

  writeChan iChan IFinished
  writeChan iChan $ IFinishTime t2
  writeChan iChan $ ITotalTime (diffUTCTime t2 t1)

  writeChan dChan DShutdown
  writeChan iChan IShutdown
