{-# LANGUAGE BangPatterns #-}
module Tracy.Main where

import Control.Applicative
import Control.Lens
import Control.Concurrent
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
                 }

render :: String
       -> Int
       -> RenderConfig
       -> SceneDesc
       -> Int
       -> (Chan JobRequest -> Chan JobResponse -> IO ())
       -> Chan InfoEvent
       -> Chan DataEvent
       -> IO ()
render sceneName numBatches renderCfg s frameNum renderManager iChan dChan = do
  let w = s^.sceneDescWorld
      rowsPerBatch = 100 -- fromEnum $ w^.wdViewPlane.vpVres
      samplesPerBatch = fromEnum $ (renderCfg^.sampleRoot) ** 2
      allRows = [0..fromEnum (w^.wdViewPlane.vpVres-1)]
      allSampleIndices = [0..fromEnum (((renderCfg^.sampleRoot) ** 2) - 1)]

      sampleRequests :: (Int, Int) -> [Int] -> [JobRequest]
      sampleRequests _ [] = []
      sampleRequests rowRange is = RenderRequest rowRange (firstI, lastI) : nextRs
          where
            firstI = head current
            lastI = head $ reverse current
            current = take samplesPerBatch is
            rest = drop samplesPerBatch is
            nextRs = sampleRequests rowRange rest

      reqs' :: (Int, Int) -> [JobRequest]
      reqs' (firstRow, lastRow) = sampleRequests (firstRow, lastRow) allSampleIndices

      reqs :: [Int] -> [JobRequest]
      reqs rs = concat (reqs' <$> rowRanges rs)

      rowRanges [] = []
      rowRanges rs = (firstRow, lastRow) : rowRanges rest
          where
            firstRow = head current
            lastRow = head $ reverse current
            current = take rowsPerBatch rs
            rest = drop rowsPerBatch rs

      requests = reqs allRows

  writeChan iChan $ ISceneName sceneName
  writeChan iChan $ IFrameNum frameNum

  writeChan dChan $ DSceneName sceneName
  writeChan dChan $ DFrameNum frameNum

  writeChan iChan $ ISampleRoot $ renderCfg^.sampleRoot
  writeChan iChan $ IAccelScheme $ s^.sceneDescAccelScheme
  writeChan iChan $ INumObjects $ w^.wdObjects.to length
  writeChan iChan $ IShadows $ w^.wdWorldShadows
  writeChan iChan $ INumBatches numBatches
  writeChan iChan $ IImageSize (fromEnum $ w^.wdViewPlane.vpHres)
                               (fromEnum $ w^.wdViewPlane.vpVres)

  writeChan dChan $ DNumBatches numBatches
  writeChan dChan $ DSampleRoot $ renderCfg^.sampleRoot
  writeChan dChan $ DImageSize (fromEnum $ w^.wdViewPlane.vpHres)
                               (fromEnum $ w^.wdViewPlane.vpVres)
  writeChan dChan $ DRowRanges $ rowRanges allRows

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

  print requests

  -- Set the scene
  writeChan reqChan $ SetScene renderCfg s mg frameNum rngSeedV (rowRanges allRows)

  t1 <- getCurrentTime
  writeChan iChan $ IStartTime t1

  writeChan dChan DStarted
  writeChan iChan IStarted

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
            BatchFinished rowRange rs -> do
                t <- getCurrentTime
                let remainingTime = toEnum $ ((fromEnum $ diffUTCTime t t1) `div` (numFinished + 1)) *
                                             (length requests - (numFinished + 1))

                writeChan iChan $ IBatchFinished (numFinished + 1) (length requests) remainingTime
                writeChan dChan $ DBatchFinished rowRange rs

                if numFinished + 1 == length requests then
                   writeChan reqChan RenderFinished >> writeChan reqChan Shutdown else
                   collector $ numFinished + 1

  collector 0

  t2 <- getCurrentTime

  writeChan dChan DFinished

  writeChan iChan IFinished
  writeChan iChan $ IFinishTime t2
  writeChan iChan $ ITotalTime (diffUTCTime t2 t1)

  writeChan dChan DShutdown
  writeChan iChan IShutdown
