module Tracy.DataHandlers.GUIHandler
  ( guiHandler
  )
  where

import Control.Concurrent.Chan
import Control.Concurrent (forkIO)
import Control.Monad
import Data.IORef
import Foreign (mallocArray)
import Foreign.Storable
import Foreign.Ptr
import Data.Colour
import System.Exit

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLUT as GLUT
import Graphics.UI.GLUT (($=))

import Tracy.Types
import Tracy.Util
import Tracy.DataHandlers.FileHandler

data MyState =
    MyState { windowWidth :: Int
            , windowHeight :: Int
            }

guiHandler :: Chan DataEvent -> IO ()
guiHandler chan = do
  DSceneName sceneName <- readChan chan
  DNumFrames _ <- readChan chan
  DSampleRoot _ <- readChan chan
  DImageSize cols rows <- readChan chan

  ref <- newIORef $ MyState cols rows
  redrawRef <- newIORef False

  imageArray <- mallocArray $ cols * rows

  _ <- GLUT.getArgsAndInitialize
  GLUT.initialDisplayMode $= [ GLUT.SingleBuffered, GLUT.RGBMode ]
  GLUT.initialWindowSize $= GL.Size (toEnum $ fromEnum cols) (toEnum $ fromEnum rows)
  GLUT.initialWindowPosition $= GL.Position 100 100
  _ <- GLUT.createWindow sceneName

  GLUT.displayCallback $= display ref imageArray
  GLUT.reshapeCallback $= Just reshape
  GLUT.keyboardMouseCallback $= Just handleKM

  let updateIntervalMs = 100
      updater = do
        checkForChanges redrawRef
        GLUT.addTimerCallback updateIntervalMs updater

  GLUT.addTimerCallback updateIntervalMs updater

  GL.clearColor $= GL.Color4 100 100 200 0
  GL.shadeModel $= GL.Flat
  GL.rowAlignment GL.Unpack $= 1

  combinedArray@(_, _, combinedPtr) <- createMergeBuffer rows cols

  let work numSamples = do
        ev <- readChan chan
        case ev of
            DFrameFinished rs -> do
                mergeFrames numSamples combinedArray rs

                forM_ [0..rows*cols-1] $ \i -> do
                    val <- peekElemOff (castPtr combinedPtr) i
                    pokeElemOff imageArray i $ toColor3 val

                writeIORef redrawRef True
                work (numSamples + 1)

            DFinished -> return ()
            DShutdown -> return ()
            _ -> work numSamples

  _ <- forkIO $ do
    DStarted <- readChan chan
    work (0 :: Int)
    vec <- vectorFromMergeBuffer combinedArray
    writeImage vec rows cols (sceneName ++ ".bmp")

  GLUT.mainLoop

  return ()

checkForChanges :: IORef Bool -> IO ()
checkForChanges ref = do
    v <- atomicModifyIORef ref (\x -> (False, x))
    when v $ GLUT.postRedisplay Nothing

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

display :: IORef MyState -> Ptr (GL.Color3 GL.GLubyte) -> GLUT.DisplayCallback
display ref imageData = do
    st <- readIORef ref

    let rasterPos2i = GLUT.rasterPos :: GL.Vertex2 GL.GLint -> IO ()
        sz = GL.Size (toEnum $ windowWidth st)
                     (toEnum $ windowHeight st)

    GL.clear [GL.ColorBuffer]
    rasterPos2i (GL.Vertex2 0 0)
    let img = GL.PixelData GL.RGB GL.UnsignedByte imageData
    GL.drawPixels sz img
    GL.flush

toColor3 :: Colour -> GL.Color3 GL.GLubyte
toColor3 (Colour r g b) = GL.Color3 (toEnum $ fromEnum (r * 255.0))
                                    (toEnum $ fromEnum (g * 255.0))
                                    (toEnum $ fromEnum (b * 255.0))
