module Tracy.GUIHandler where

import Control.Applicative
import Control.Concurrent.Chan
import Control.Concurrent.STM
import Control.Concurrent (forkIO)
import Control.Monad
import Control.Monad.State
import Data.IORef
import Foreign (newArray)
import Data.Colour
import System.Exit

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLUT as GLUT
import Graphics.UI.GLUT (($=))

import Tracy.Types

type App = StateT MyState IO

type Image = GL.PixelData (GL.Color3 GL.GLubyte)

data MyState =
    MyState { completed :: [(Int, [[Colour]])]
            , windowWidth :: Int
            , windowHeight :: Int
            }

guiHandler :: Chan DataEvent -> IO ()
guiHandler chan = do
  DSceneName sceneName <- readChan chan
  DNumChunks chunks <- readChan chan
  DImageSize cols rows <- readChan chan

  ref <- newIORef $ MyState [] cols rows

  updateChan <- atomically newTChan

  _ <- GLUT.getArgsAndInitialize
  GLUT.initialDisplayMode $= [ GLUT.SingleBuffered, GLUT.RGBMode ]
  GLUT.initialWindowSize $= GL.Size (toEnum $ fromEnum cols) (toEnum $ fromEnum rows)
  GLUT.initialWindowPosition $= GL.Position 100 100
  _ <- GLUT.createWindow sceneName

  GLUT.displayCallback $= display ref
  GLUT.idleCallback $= (Just $ checkForChanges updateChan)
  GLUT.reshapeCallback $= Just reshape
  GLUT.keyboardMouseCallback $= Just handleKM

  GL.clearColor $= GL.Color4 100 100 200 0
  GL.shadeModel $= GL.Flat
  GL.rowAlignment GL.Unpack $= 1

  _ <- forkIO $ do
    DStarted <- readChan chan

    forM_ [1..chunks] $ \_ -> do
        DChunkFinished ch rs <- readChan chan
        modifyIORef' ref $ \s -> s { completed = completed s ++ [(ch, rs)] }
        atomically $ writeTChan updateChan ()

    DFinished <- readChan chan
    return ()

  GLUT.mainLoop

  return ()

checkForChanges :: TChan () -> GLUT.IdleCallback
checkForChanges ch = do
    v <- atomically $ tryReadTChan ch
    case v of
        Nothing -> return ()
        Just _ -> GLUT.postRedisplay Nothing

handleKM :: GLUT.KeyboardMouseCallback
handleKM key _updown _mods _pos =
    case key of
        GLUT.Char 'q' -> exitSuccess
        _ -> return ()

reshape :: GLUT.ReshapeCallback
reshape size@(GL.Size w h) = do
    GL.viewport $= (GL.Position 0 0, size)
    GL.matrixMode $= GL.Projection
    GL.loadIdentity
    GL.ortho2D 0 (fromIntegral w) 0 (fromIntegral h)
    GL.matrixMode $= GL.Modelview 0
    GL.loadIdentity

display :: IORef MyState -> GLUT.DisplayCallback
display ref = do
    st <- readIORef ref

    let rasterPos2i = GLUT.rasterPos :: GL.Vertex2 GL.GLint -> IO ()
        sz = GL.Size w h
        w = toEnum $ fromEnum $ windowWidth st
        h = toEnum $ sum $ length <$> snd <$> completed st

    GL.clear [GL.ColorBuffer]
    rasterPos2i (GL.Vertex2 0 0)
    GL.drawPixels sz =<< mkRenderImage (completed st)
    GL.flush

mkRenderImage :: [(Int, [[Colour]])] -> IO Image
mkRenderImage raw = do
    let vals = toColor3 <$> (concat $ concat (snd <$> raw))
        toColor3 :: Colour -> GL.Color3 GL.GLubyte
        toColor3 (Colour r g b) = GL.Color3 (toEnum $ fromEnum (r * 255.0))
                                            (toEnum $ fromEnum (g * 255.0))
                                            (toEnum $ fromEnum (b * 255.0))
    (GL.PixelData GL.RGB GL.UnsignedByte) <$> (newArray vals)
