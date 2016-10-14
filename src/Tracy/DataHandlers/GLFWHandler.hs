module Tracy.DataHandlers.GLFWHandler
  ( glfwHandler
  )
  where

import Control.Applicative
import Control.Concurrent.Chan
import Control.Concurrent
import Control.Monad
import qualified Data.Map as M
import qualified Data.Vector.Storable as SV
import Data.IORef
import Foreign (mallocArray)
import Foreign.Storable
import Foreign.Ptr
import Data.Colour
import System.IO (hPutStrLn, stderr)

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.UI.GLFW as G

import Tracy.Types
import Tracy.Util
import Tracy.DataHandlers.FileHandler

data MyState =
    MyState { windowWidth :: Int
            , windowHeight :: Int
            }

errorCallback :: G.ErrorCallback
errorCallback _err description = hPutStrLn stderr description

withGLFWInit :: IO () -> IO ()
withGLFWInit act = do
  G.setErrorCallback (Just errorCallback)
  successfulInit <- G.init
  if successfulInit then act else return ()

withWindow :: IO (Maybe G.Window) -> (G.Window -> IO ()) -> IO ()
withWindow mkWindow f = do
    w <- mkWindow
    case w of
        Nothing -> return ()
        Just w' -> f w'

windowTitle :: Int -> String -> String
windowTitle fn sceneName = sceneName ++ " (frame " ++ show fn ++ ")"

glfwHandler :: MVar () -> Chan DataEvent -> IO ()
glfwHandler stopMvar chan = withGLFWInit $ do
  DSceneName sceneName <- readChan chan
  DSampleRoot _ <- readChan chan
  DImageSize (Width cols) (Height rows) <- readChan chan
  DRowRanges rowRanges <- readChan chan
  DFrameRange (firstFrame, lastFrame) <- readChan chan

  (frameWriter, finishOutput) <- setupFrameOutput (firstFrame, lastFrame) (cols, rows) sceneName

  ref <- newIORef $ MyState cols rows

  imageArray <- mallocArray $ cols * rows
  combinedArray@(_, _, combinedPtr) <- createMergeBuffer rows cols

  -- Set up window hints
  G.windowHint $ G.WindowHint'sRGBCapable True
  G.windowHint $ G.WindowHint'Resizable False

  withWindow (G.createWindow cols rows sceneName Nothing Nothing) $ \window -> do
    let sCountMap = M.fromList $ zip (fst <$> rowRanges) $ repeat (0::Int)
        work sampleCounts = do
          shouldClose <- G.windowShouldClose window
          shouldStop <- not <$> isEmptyMVar stopMvar
          when (not $ shouldClose || shouldStop) $ do
              ev <- readChan chan
              next <- case ev of
                  DStarted (Frame fn) -> do
                      G.setWindowTitle window $ windowTitle fn sceneName
                      return $ Just $ work sampleCounts
                  DChunkFinished (startRow@(Row startRowI), Row stopRow) (Count sc) rs -> do
                      let numSamples = sampleCounts M.! startRow
                          newSampleCount = numSamples + sc
                          startIndex = startRowI * cols
                          stopIndex = ((stopRow + 1) * cols) - 1

                      mergeChunks numSamples newSampleCount startRow combinedArray rs

                      forM_ [startIndex..stopIndex] $ \i -> do
                          val <- peekElemOff (castPtr combinedPtr) i
                          pokeElemOff imageArray i $! toColor3 $! maxToOne val

                      display window ref imageArray
                      G.swapBuffers window

                      return $ Just $ work $
                        M.alter (\(Just v) -> Just (v + sc)) startRow sampleCounts

                  DFinished frameNum -> do
                      -- Write the current accumulation buffer to disk
                      vec <- vectorFromMergeBuffer combinedArray
                      let vec2 = SV.map maxToOne vec
                      frameWriter vec2 frameNum

                      -- If we just wrote the last frame in the sequence,
                      -- shut down the output stream
                      when (frameNum == lastFrame) finishOutput

                      -- Start over with a new sample count map
                      return $ Just $ work sCountMap
                  DShutdown -> do
                      let waitForQuit = do
                              wsc <- G.windowShouldClose window
                              e <- not <$> isEmptyMVar stopMvar
                              when (not $ wsc || e) $ G.pollEvents >> threadDelay 100000 >> waitForQuit
                      waitForQuit
                      return Nothing
                  _ -> return $ Just $ work sampleCounts

              G.pollEvents
              case next of
                  Nothing -> return ()
                  Just act -> act

    G.setKeyCallback window (Just handleKeys)
    G.setFramebufferSizeCallback window (Just resizeFb)

    (fbx, fby) <- G.getFramebufferSize window
    resizeFb window fbx fby

    GL.clearColor $= GL.Color4 100 100 200 0
    GL.shadeModel $= GL.Flat
    GL.rowAlignment GL.Unpack $= 1

    work sCountMap
    G.destroyWindow window
    G.terminate

resizeFb :: G.Window -> Int -> Int -> IO ()
resizeFb _ w h = do
    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
    GL.matrixMode $= GL.Projection
    GL.loadIdentity
    GL.ortho2D 0 (fromIntegral w) 0 (fromIntegral h)
    GL.matrixMode $= GL.Modelview 0
    GL.loadIdentity

handleKeys :: G.KeyCallback
handleKeys w key _ _ _ =
    case key of
        G.Key'Q -> G.setWindowShouldClose w True
        _ -> return ()

display :: G.Window -> IORef MyState -> Ptr (GL.Color3 GL.GLubyte) -> IO ()
display window ref imageData = do
    st <- readIORef ref

    let rasterPos2i = GL.rasterPos :: GL.Vertex2 GL.GLint -> IO ()
        sz = GL.Size (toEnum $ windowWidth st)
                     (toEnum $ windowHeight st)

    (wx, wy) <- G.getWindowSize window
    (fbx, fby) <- G.getFramebufferSize window
    let (xZoom, yZoom) = (toEnum fbx / toEnum wx, toEnum fby / toEnum wy)
    GL.pixelZoom $= (xZoom, yZoom)

    GL.clear [GL.ColorBuffer]
    rasterPos2i (GL.Vertex2 (-1) (-1))
    let img = GL.PixelData GL.RGB GL.UnsignedByte imageData
    GL.drawPixels sz img

toColor3 :: Colour -> GL.Color3 GL.GLubyte
toColor3 (Colour r g b) = GL.Color3 (toEnum $ fromEnum (r * 255.0))
                                    (toEnum $ fromEnum (g * 255.0))
                                    (toEnum $ fromEnum (b * 255.0))
