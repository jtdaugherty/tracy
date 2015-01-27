{-# LANGUAGE BangPatterns #-}
module Tracy.Main where

import Control.Lens
import Control.Concurrent
import Data.Time.Clock
import System.Exit
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
      requests = replicate numBatches RenderRequest

  writeChan iChan $ ISceneName sceneName
  writeChan iChan $ IFrameNum frameNum

  writeChan dChan $ DSceneName sceneName
  writeChan dChan $ DFrameNum frameNum

  writeChan iChan $ ISampleRoot $ renderCfg^.sampleRoot
  writeChan iChan $ IAccelScheme $ s^.sceneDescAccelScheme
  writeChan iChan $ INumObjects $ w^.wdObjects.to length
  writeChan iChan $ IShadows $ w^.wdWorldShadows
  writeChan iChan $ INumBatches numBatches
  writeChan iChan $ IImageSize (fromEnum $ w^.wdViewPlane.hres)
                               (fromEnum $ w^.wdViewPlane.vres)

  writeChan dChan $ DNumBatches numBatches
  writeChan dChan $ DSampleRoot $ renderCfg^.sampleRoot
  writeChan dChan $ DImageSize (fromEnum $ w^.wdViewPlane.hres)
                               (fromEnum $ w^.wdViewPlane.vres)

  writeChan iChan ILoadingMeshes

  -- Preload meshes
  !mg <- loadMeshes s

  writeChan iChan $ ILoadedMeshes $ M.size mg

  reqChan <- newChan
  respChan <- newChan

  -- Start the renderer thread
  _ <- forkIO (renderManager reqChan respChan)

  -- Set the scene
  writeChan reqChan $ SetScene renderCfg s mg frameNum

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
            BatchFinished rs -> do
                t <- getCurrentTime
                let remainingTime = toEnum $ ((fromEnum $ diffUTCTime t t1) `div` (numFinished + 1)) *
                                             (numBatches - (numFinished + 1))

                writeChan iChan $ IBatchFinished (numFinished + 1) numBatches remainingTime
                writeChan dChan $ DBatchFinished rs

                if numFinished + 1 == numBatches then
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
