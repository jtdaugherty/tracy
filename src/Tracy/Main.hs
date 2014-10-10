module Tracy.Main where

import Control.Lens
import Control.Concurrent
import Data.Time.Clock
import System.Exit

import Tracy.Types

defaultRenderConfig :: RenderConfig
defaultRenderConfig =
    RenderConfig { _sampleRoot = 4
                 , _forceShadows = Nothing
                 }

render :: String
       -> Int
       -> RenderConfig
       -> SceneDesc
       -> (Chan JobRequest -> Chan JobResponse -> IO ())
       -> Chan InfoEvent
       -> Chan DataEvent
       -> IO ()
render sceneName numFrames renderCfg s renderManager iChan dChan = do
  let w = s^.sceneDescWorld
      requests = replicate numFrames RenderRequest

  writeChan iChan $ ISceneName sceneName
  writeChan dChan $ DSceneName sceneName

  writeChan iChan $ ISampleRoot $ renderCfg^.sampleRoot
  writeChan iChan $ IAccelScheme $ s^.sceneDescAccelScheme
  writeChan iChan $ INumObjects $ w^.wdObjects.to length
  writeChan iChan $ IShadows $ w^.wdWorldShadows
  writeChan iChan $ INumFrames numFrames
  writeChan iChan $ IImageSize (fromEnum $ w^.wdViewPlane.hres)
                               (fromEnum $ w^.wdViewPlane.vres)

  writeChan dChan $ DNumFrames numFrames
  writeChan dChan $ DSampleRoot $ renderCfg^.sampleRoot
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
  writeChan reqChan $ SetScene renderCfg s

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
            FrameFinished rs -> do
                t <- getCurrentTime
                let remainingTime = toEnum $ ((fromEnum $ diffUTCTime t t1) `div` (numFinished + 1)) *
                                             (numFrames - (numFinished + 1))

                writeChan iChan $ IFrameFinished (numFinished + 1) numFrames remainingTime
                writeChan dChan $ DFrameFinished rs

                if numFinished + 1 == numFrames then
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
