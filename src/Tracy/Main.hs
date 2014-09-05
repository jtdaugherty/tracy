module Tracy.Main where

import Control.Applicative
import Control.Lens
import Control.Concurrent
import Data.Time.Clock
import Data.Colour
import System.Exit

import Tracy.Types

defaultRenderConfig :: RenderConfig
defaultRenderConfig =
    RenderConfig { _sampleRoot = 4
                 , _accelScheme = NoScheme
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
            ChunkFinished chunkId startStop rs -> do
                t <- getCurrentTime
                let remainingTime = toEnum $ ((fromEnum $ diffUTCTime t t1) `div` (numFinished + 1)) *
                                             (length chunks - (numFinished + 1))
                    converted = (cpromote <$>) <$> rs
                writeChan iChan $ IChunkFinished chunkId (length chunks) remainingTime
                writeChan dChan $ DChunkFinished chunkId startStop converted
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
