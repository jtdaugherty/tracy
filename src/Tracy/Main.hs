{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tracy.Main where

import Control.Parallel.Strategies
import Control.Lens
import Control.DeepSeq
import Control.Monad
import Control.Concurrent
import Data.Time.Clock
import Data.Colour
import Data.Serialize
import qualified Data.Vector as V
import System.Random
import System.Exit
import System.ZMQ4

import Tracy.Types
import Tracy.Samplers
import Tracy.SceneBuilder

defaultRenderConfig :: RenderConfig
defaultRenderConfig =
    RenderConfig { _sampleRoot = 4
                 , _accelScheme = NoScheme
                 , _forceShadows = Nothing
                 }

instance NFData Colour where
    rnf (Colour r g b) = r `seq` g `seq` b `seq` ()

render :: String
       -> Int
       -> RenderConfig
       -> SceneDesc
       -> (Chan JobRequest -> Chan JobResponse -> IO ())
       -> Chan InfoEvent
       -> Chan DataEvent
       -> IO ()
render sceneName requestedChunks renderCfg s renderManager iChan dChan = do
  let w = s^.sceneDescWorld
      -- Number of pixel rows per chunk
      rowsPerChunk = w^.wdViewPlane.vres / (toEnum requestedChunks)
      -- Chunk a list up by a function that gives us the first chunk
      chunk f xs = result : chunk f rest where (result, rest) = f xs
      -- These are the rows we will render
      rows = [0..(fromEnum $ w^.wdViewPlane.vres-1)]
      -- Then we split up the rows into groups (chunks) based on the chunk size
      chunks = filter (not . null) $ take (requestedChunks + 1) $
                                     chunk (splitAt (fromEnum rowsPerChunk)) rows
      numChunks = length chunks
      -- For each chunk, construct a rendering request for the relevant row
      -- range and chunk ID
      requests = [ RenderRequest i (ch !! 0, ch !! (length ch - 1))
                 | (i, ch) <- zip [1..] chunks
                 ]
      numSets = fromEnum (w^.wdViewPlane.hres * 2.3)
      squareSampler = regular
      -- XXX this *should* be taken from the camera, but we don't have one of
      -- those at this stage.  If we ever start using other camera types, this
      -- could be a problem.
      diskSampler = toUnitDisk jittered

  -- Generate sample data for square and disk samplers
  squareSamples <- replicateM numSets $ squareSampler (renderCfg^.sampleRoot)
  diskSamples <- replicateM numSets $ diskSampler (renderCfg^.sampleRoot)

  writeChan iChan $ ISceneName sceneName
  writeChan dChan $ DSceneName sceneName

  writeChan iChan $ ISampleRoot $ renderCfg^.sampleRoot
  writeChan iChan $ IAccelScheme $ s^.sceneDescAccelScheme
  writeChan iChan $ INumObjects $ w^.wdObjects.to length
  writeChan iChan $ IShadows $ w^.wdWorldShadows
  writeChan iChan $ INumRowsPerChunk $ fromEnum rowsPerChunk
  writeChan iChan $ INumChunks $ length chunks
  writeChan iChan $ IImageSize (fromEnum $ w^.wdViewPlane.hres)
                               (fromEnum $ w^.wdViewPlane.vres)

  writeChan dChan $ DNumChunks $ length chunks
  writeChan dChan $ DImageSize (fromEnum $ w^.wdViewPlane.hres)
                               (fromEnum $ w^.wdViewPlane.vres)

  t1 <- getCurrentTime
  writeChan iChan $ IStartTime t1

  writeChan dChan DStarted
  writeChan iChan IStarted

  reqChan <- newChan
  respChan <- newChan

  -- Start the renderer thread
  _ <- forkIO (renderManager reqChan respChan)

  -- Set the scene
  writeChan reqChan $ SetScene renderCfg s squareSamples diskSamples

  -- Send the rendering requests
  mapM_ (writeChan reqChan) requests

  -- Wait for the responses
  let collector numFinished = do
        resp <- readChan respChan
        case resp of
            JobError msg -> do
                putStrLn $ "Yikes! Error in render thread: " ++ msg
                exitSuccess
            JobAck -> collector numFinished
            ChunkFinished chunkId startStop rs -> do
                t <- getCurrentTime
                let remainingTime = toEnum $ ((fromEnum $ diffUTCTime t t1) `div` (numFinished + 1)) *
                                             (length chunks - (numFinished + 1))
                writeChan iChan $ IChunkFinished chunkId (length chunks) remainingTime
                writeChan dChan $ DChunkFinished chunkId startStop rs
                if numFinished + 1 == numChunks then
                   writeChan reqChan RenderFinished else
                   collector $ numFinished + 1

  collector 0

  t2 <- getCurrentTime

  writeChan dChan DFinished

  writeChan iChan IFinished
  writeChan iChan $ IFinishTime t2
  writeChan iChan $ ITotalTime (diffUTCTime t2 t1)

  writeChan dChan DShutdown
  writeChan iChan IShutdown

localRenderThread :: Chan JobRequest -> Chan JobResponse -> IO ()
localRenderThread jobReq jobResp = do
    let waitForJob = do
          ev <- readChan jobReq
          case ev of
              SetScene cfg sDesc sSamples dSamples -> do
                  let Right s = sceneFromDesc sDesc
                      aScheme = s^.sceneAccelScheme
                      worldAccel = (aScheme^.schemeApply) (s^.sceneWorld)
                      worldAccelShadows = case cfg^.forceShadows of
                                            Nothing -> worldAccel
                                            Just v -> worldAccel & worldShadows .~ v
                      newScene = s & sceneWorld .~ worldAccelShadows
                  writeChan jobResp JobAck
                  processRequests cfg newScene sSamples dSamples
                  waitForJob
              Shutdown -> do
                  writeChan jobResp JobAck
              _ -> writeChan jobResp $ JobError "Expected SetScene or Shutdown, got unexpected event"

        processRequests cfg s sSamples dSamples = do
          ev <- readChan jobReq
          case ev of
              RenderRequest chunkId (start, stop) -> do
                  ch <- renderChunk cfg s (start, stop) sSamples dSamples
                  writeChan jobResp $ ChunkFinished chunkId (start, stop) ch
                  processRequests cfg s sSamples dSamples
              RenderFinished -> do
                  writeChan jobResp JobAck
              _ -> writeChan jobResp $ JobError "Expected RenderRequest or RenderFinished, got unexpected event"

    waitForJob

networkNodeThread :: String -> Chan InfoEvent -> Chan JobRequest -> Chan JobResponse -> IO () -> IO ()
networkNodeThread connStr iChan jobReq jobResp readyNotify = withContext $ \ctx -> do
    sock <- socket ctx Req
    connect sock connStr
    writeChan iChan $ IConnected connStr

    readyNotify

    let worker = do
          ev <- readChan jobReq
          case ev of
              SetScene cfg s sSamples dSamples -> do
                  send sock [] $ encode $ SetScene cfg s sSamples dSamples
                  _ <- receive sock
                  worker
              RenderRequest chunkId (start, stop) -> do
                  send sock [] $ encode $ RenderRequest chunkId (start, stop)
                  reply <- receive sock
                  case decode reply of
                      Left e -> writeChan jobResp $ JobError e
                      Right r -> writeChan jobResp r
                  readyNotify
                  worker
              RenderFinished -> do
                  send sock [] $ encode RenderFinished
                  _ <- receive sock
                  worker
              Shutdown -> do
                  send sock [] $ encode Shutdown
                  _ <- receive sock
                  return ()

    worker

networkRenderThread :: [String] -> Chan InfoEvent -> Chan JobRequest -> Chan JobResponse -> IO ()
networkRenderThread nodes iChan jobReq jobResp = do
    reqChans <- replicateM (length nodes) newChan
    readyChan <- newChan

    -- Connect to all nodes
    forM_ (zip3 nodes reqChans [0..]) $ \(n, ch, i) -> do
        forkIO $ networkNodeThread n iChan ch jobResp (writeChan readyChan i)

    let sendToAll val = forM_ reqChans $ \ch -> writeChan ch val
        chanReader = do
            req <- readChan jobReq
            case req of
                SetScene cfg s sSamples dSamples -> do
                    sendToAll $ SetScene cfg s sSamples dSamples
                    chanReader
                RenderRequest chunkId (start, stop) -> do
                    -- Find available (non-busy) node
                    nodeId <- readChan readyChan
                    -- Send the request to its channel
                    writeChan (reqChans !! nodeId) $ RenderRequest chunkId (start, stop)
                    chanReader
                RenderFinished -> do
                    sendToAll RenderFinished
                    chanReader
                Shutdown -> do
                    sendToAll Shutdown

    chanReader

renderChunk :: RenderConfig -> Scene ThinLens -> (Int, Int) -> [[(Float, Float)]] -> [[(Float, Float)]] -> IO [[Color]]
renderChunk cfg s (start, stop) sSamples dSamples = do
  let cam = s^.sceneCamera
      w = s^.sceneWorld
      numSets = fromEnum (w^.viewPlane.hres * 2.3)
      renderer = cam^.cameraRenderWorld
      chunkRows = [start..stop]
      squareSamples = V.fromList sSamples
      diskSamples = V.fromList dSamples
      worker = renderer cam numSets cfg w squareSamples diskSamples

  -- Zip up chunkRows values with sets of randomly-generated sample set indices
  sampleIndices <- timeIt "sampleIndices" $ replicateM (stop - start + 1) $
                                              replicateM numSets $
                                                randomRIO (0, numSets - 1)

  let r = parMap (rpar `dot` rdeepseq) worker (zip chunkRows sampleIndices)
  r `deepseq` return ()

  return r
