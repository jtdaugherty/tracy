module Tracy.GUIHandler
  ( guiHandler
  )
  where

import Control.Applicative
import Control.Concurrent.Chan
import Control.Concurrent.STM
import Control.Concurrent (forkIO)
import Control.Monad
import Data.IORef
import Foreign (Ptr, mallocArray, advancePtr, pokeArray)
import Data.Colour
import System.Exit

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLUT as GLUT
import Graphics.UI.GLUT (($=))

import Tracy.Types

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
  imageArray <- mallocArray $ cols * rows

  updateChan <- atomically newTChan

  _ <- GLUT.getArgsAndInitialize
  GLUT.initialDisplayMode $= [ GLUT.SingleBuffered, GLUT.RGBMode ]
  GLUT.initialWindowSize $= GL.Size (toEnum $ fromEnum cols) (toEnum $ fromEnum rows)
  GLUT.initialWindowPosition $= GL.Position 100 100
  _ <- GLUT.createWindow sceneName

  GLUT.displayCallback $= display ref imageArray
  GLUT.idleCallback $= (Just $ checkForChanges updateChan)
  GLUT.reshapeCallback $= Just reshape
  GLUT.keyboardMouseCallback $= Just handleKM

  GL.clearColor $= GL.Color4 100 100 200 0
  GL.shadeModel $= GL.Flat
  GL.rowAlignment GL.Unpack $= 1

  _ <- forkIO $ do
    DStarted <- readChan chan

    forM_ [1..chunks] $ \_ -> do
        DChunkFinished ch (start, _) rs <- readChan chan
        -- Create a new pointer from imageArray with the appropriate chunk offset
        let offPtr = advancePtr imageArray $ start * cols
        -- Poke pixel data into the array at that position
        pokeArray offPtr $ toColor3 <$> concat rs
        modifyIORef' ref $ \s -> s { completed = (ch, rs) : completed s }
        atomically $ writeTChan updateChan ()

    DFinished <- readChan chan
    DShutdown <- readChan chan
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
