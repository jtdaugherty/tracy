module Tracy.GUIHandler where

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent (forkIO)
import Control.Monad

import qualified Graphics.UI.GLFW as GLFW

import Tracy.Types
import Tracy.FileHandler

withFanout :: [Chan a -> IO ()] -> ([Chan a] -> IO ()) -> IO ()
withFanout actions body = do
  chans <- replicateM (length actions) newChan
  mvars <- replicateM (length actions) newEmptyMVar
  forM_ (zip3 actions chans mvars) $ \(a, c, m) ->
      forkIO (a c) >> putMVar m ()
  body chans
  mapM_ takeMVar mvars

guiFileHandler :: FilePath -> Chan DataEvent -> IO ()
guiFileHandler path chan = do
  let processEvents chans = do
        ev <- readChan chan
        mapM_ (flip writeChan ev) chans
        if ev == DShutdown then
          return () else
          processEvents chans

  withFanout [fileHandler path, guiHandler] processEvents

guiHandler :: Chan DataEvent -> IO ()
guiHandler chan = do
  DNumChunks chunks <- readChan chan
  DImageSize cols rows <- readChan chan

  withWindow cols rows "untitled" $ \win -> do
    DStarted <- readChan chan

    forM_ [1..chunks] $ \_ -> do
        DChunkFinished ch rs <- readChan chan
        return ()

    DFinished <- readChan chan

    -- XXX: wait for user input before we shut down and close the window
    return ()

withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow width height title f = do
    GLFW.setErrorCallback $ Just simpleErrorCallback
    r <- GLFW.init
    when r $ do
        m <- GLFW.createWindow width height title Nothing Nothing
        case m of
          (Just win) -> do
              GLFW.makeContextCurrent m
              f win
              GLFW.setErrorCallback $ Just simpleErrorCallback
              GLFW.destroyWindow win
          Nothing -> return ()
        GLFW.terminate
  where
    simpleErrorCallback e s =
        putStrLn $ unwords [show e, show s]
