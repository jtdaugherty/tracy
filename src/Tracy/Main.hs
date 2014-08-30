{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tracy.Main where

import Control.Parallel.Strategies
import Control.Lens
import Control.DeepSeq
import Control.Monad
import Control.Concurrent.Chan
import Data.Time.Clock
import Data.Colour
import Data.Array.IO
import qualified Data.Vector as V
import GHC.Conc
import System.Random
import System.Exit

import Tracy.Types
import Tracy.Samplers
import Tracy.Cameras
import Tracy.AccelSchemes

defaultConfig :: IO Config
defaultConfig = do
    n <- getNumProcessors
    return $ Config { _vpSampler = regular
                    , _sampleRoot = 4
                    , _accelScheme = noScheme
                    , _cpuCount = n
                    , _workChunks = 10
                    }

instance NFData Colour where
    rnf (Colour r g b) = r `seq` g `seq` b `seq` ()

shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs

localRender :: String -> Config -> Scene ThinLens -> Chan InfoEvent -> Chan DataEvent -> IO ()
localRender sceneName cfg s iChan dChan = do
  let w = s^.sceneWorld
      numChunks = cfg^.workChunks
      rowsPerChunk = w^.viewPlane.vres / (toEnum numChunks)
      chunk f xs = result : chunk f rest where (result, rest) = f xs
      chunks = filter (not . null) $ take (numChunks + 1) $ chunk (splitAt (fromEnum rowsPerChunk)) rows
      rows = [0..(fromEnum $ w^.viewPlane.vres-1)]
      requests = [ RenderRequest i (ch !! 0, ch !! (length ch - 1))
                 | (i, ch) <- zip [1..] chunks
                 ]

  writeChan iChan $ ISceneName sceneName
  writeChan dChan $ DSceneName sceneName

  writeChan iChan $ ISampleRoot $ cfg^.sampleRoot
  writeChan iChan $ IAccelSchemeName (cfg^.accelScheme.schemeName)
  writeChan iChan $ INumObjects $ w^.objects.to length
  writeChan iChan $ IShadows $ w^.worldShadows
  writeChan iChan $ INumCPUs $ cfg^.cpuCount
  writeChan iChan $ INumRowsPerChunk $ fromEnum rowsPerChunk
  writeChan iChan $ INumChunks $ length chunks
  writeChan iChan $ IImageSize (fromEnum $ w^.viewPlane.hres)
                               (fromEnum $ w^.viewPlane.vres)

  writeChan dChan $ DNumChunks $ length chunks
  writeChan dChan $ DImageSize (fromEnum $ w^.viewPlane.hres)
                               (fromEnum $ w^.viewPlane.vres)

  t1 <- getCurrentTime
  writeChan iChan $ IStartTime t1

  writeChan dChan DStarted
  writeChan iChan IStarted

  -- Start renderThread
  -- Send it a scene set request
  -- Send it a bunch of chunk requests
  -- As responses come in, send them out to the dChan and iChan
  -- Once we have them all, send finished message, then shutdown message

  reqChan <- newChan
  respChan <- newChan

  -- Start the renderer thread
  _ <- forkIO (renderThread reqChan respChan)

  -- Set the scene
  writeChan reqChan $ SetScene cfg s

  -- Send the rendering requests
  mapM_ (writeChan reqChan) requests

  -- Wait for the responses
  results <- forM_ requests $ \_ -> do
               resp <- readChan respChan
               case resp of
                   JobError s -> do
                       putStrLn $ "Yikes! Error in render thread: " ++ s
                       exitSuccess
                   ChunkFinished chunkId rs -> do
                       t <- getCurrentTime
                       let remaining = toEnum $ ((fromEnum $ diffUTCTime t t1) `div` chunkId) * (length chunks - chunkId)
                           estimate = if chunkId == 1
                                      then Nothing
                                      else Just remaining
                       writeChan iChan $ IChunkFinished chunkId (length chunks) estimate
                       writeChan dChan $ DChunkFinished chunkId rs

  t2 <- getCurrentTime

  writeChan dChan DFinished

  writeChan iChan IFinished
  writeChan iChan $ IFinishTime t2
  writeChan iChan $ ITotalTime (diffUTCTime t2 t1)

  writeChan dChan DShutdown
  writeChan iChan IShutdown

renderThread :: Chan JobRequest -> Chan JobResponse -> IO ()
renderThread jobReq jobResp = do
    let waitForJob = do
          ev <- readChan jobReq
          case ev of
              SetScene cfg s -> do
                  processRequests cfg s
                  waitForJob
              Shutdown -> return ()
              _ -> writeChan jobResp $ JobError "Expected SetScene or Shutdown, got unexpected event"

        processRequests cfg s = do
          ev <- readChan jobReq
          case ev of
              RenderRequest chunkId (start, stop) -> do
                  ch <- renderChunk cfg s (start, stop)
                  writeChan jobResp $ ChunkFinished chunkId ch
                  processRequests cfg s
              RenderFinished -> return ()
              _ -> writeChan jobResp $ JobError "Expected RenderRequest or RenderFinished, got unexpected event"

    waitForJob

renderChunk :: Config -> Scene ThinLens -> (Int, Int) -> IO [[Color]]
renderChunk cfg s (start, stop) = do
  let cam = s^.sceneCamera
      w = s^.sceneWorld
      numSets = fromEnum (w^.viewPlane.hres * 2.3)
      squareSampler = cfg^.vpSampler
      diskSampler = cam^.cameraData.lensSampler
      renderer = cam^.cameraRenderWorld
      chunkRows = [start..stop]

  -- Generate sample data for square and disk samplers
  squareSamples <- V.replicateM numSets $ squareSampler (cfg^.sampleRoot)
  diskSamples <- V.replicateM numSets $ diskSampler (cfg^.sampleRoot)

  let worker = renderer cam numSets cfg w squareSamples diskSamples

  -- Zip up chunkRows values with sets of randomly-generated sample set indices
  indices <- forM chunkRows $ \_ -> shuffle [0..numSets-1]

  let r = parMap (rpar `dot` rdeepseq) worker (zip chunkRows indices)
  r `deepseq` return ()

  return r
