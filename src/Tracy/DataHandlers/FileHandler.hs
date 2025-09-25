module Tracy.DataHandlers.FileHandler
  ( fileHandler
  , writeImage
  , buildImageFilename
  , buildMovieFilename
  , setupFrameOutput
  )
  where

import Control.Concurrent.Chan
import Data.Maybe (fromJust)
import qualified Data.Map as M
import qualified Data.ByteString as B
import Codec.BMP
import qualified Data.Vector.Storable as SV
-- import Data.Word (Word8)
-- import Data.Colour
import System.IO (stderr)
import System.IO.Silently

-- import Codec.FFmpeg (initFFmpeg, defaultParams, imageWriter)
-- import qualified Codec.Picture as JP

import Tracy.Types
import Tracy.Util

buildImageFilename :: String -> Frame -> FilePath
buildImageFilename sn (Frame fn) = sn ++ "-" ++ show fn ++ ".bmp"

buildMovieFilename :: String -> Frame -> Frame -> FilePath
buildMovieFilename sn _ _ = sn ++ ".mp4"

fileHandler :: Chan DataEvent -> IO ()
fileHandler chan = do
  DSceneName sceneName <- readChan chan
  DSampleRoot _ <- readChan chan
  DImageSize (Width cols) (Height rows) <- readChan chan
  DRowRanges rowRanges <- readChan chan
  DFrameRange frameRange <- readChan chan

  (frameWriter, finishOutput) <- setupFrameOutput frameRange (cols, rows) sceneName
  merged <- createMergeBuffer rows cols

  let sCountMap = M.fromList $ zip (fst <$> rowRanges) $ repeat 0
      work m = do
        ev <- readChan chan
        case ev of
            DChunkFinished rowRange (Count sc) rs -> do
              let startRow = fst rowRange
                  oldCnt = m M.! startRow
                  newCnt = oldCnt + sc
              mergeChunks oldCnt newCnt startRow merged rs
              work $ M.alter (\val -> Just (fromJust val + sc)) startRow m
            DFinished frameNum -> do
                vec <- vectorFromMergeBuffer merged
                let vec2 = SV.map maxToOne vec
                frameWriter vec2 frameNum
                -- Reset sample count state
                work sCountMap
            DShutdown -> finishOutput >> return ()
            DStarted _ -> work m
            _ -> error "FileHandler: unexpected event!"

  work sCountMap

writeImage :: SV.Vector Color -> Int -> Int -> FilePath -> IO ()
writeImage dat cols rows filename = do
  let imgBytes = B.concat $ (getColorBytes <$> (SV.toList dat))
      img = packRGBA32ToBMP (fromEnum cols) (fromEnum rows) imgBytes

  writeBMP filename img

setupFrameOutput :: (Frame, Frame) -> (Int, Int) -> String -> IO (SV.Vector Color -> Frame -> IO (), IO ())
setupFrameOutput _ (cols, rows) sceneName = hSilence [stderr] $ do
  -- Set up frame writer: if we are rendering more than one frame,
  -- assume we are writing a movie and set up a streaming video encoder.
  -- case lastFrame > firstFrame of
  --     True -> do
  --         initFFmpeg
  --         let eps = defaultParams (toEnum $ fromEnum cols)
  --                                 (toEnum $ fromEnum rows)
  --         juicyImageWriteFunc <- imageWriter eps (buildMovieFilename sceneName firstFrame lastFrame)
  --         let writeFrame vec _ = do
  --               let img = juicyImageFromVec cols rows vec
  --               juicyImageWriteFunc $ Just img
  --         return (\v f -> hSilence [stderr] $ writeFrame v f, hSilence [stderr] $ juicyImageWriteFunc Nothing)
  --     False -> do
          let writeFrame vec fn = writeImage vec cols rows (buildImageFilename sceneName fn)
          return (writeFrame, return ())

-- juicyImageFromVec :: Int -> Int -> SV.Vector Color -> JP.Image JP.PixelRGB8
-- juicyImageFromVec w h colorVec = flipImageVertically $ JP.Image w h componentVec
--     where
--         componentVec = SV.concatMap toWordVec colorVec

-- flipImageVertically :: (JP.Pixel a) => JP.Image a -> JP.Image a
-- flipImageVertically image = JP.generateImage flippedPixel (JP.imageWidth image) (JP.imageHeight image)
--     where
--         flippedPixel x y = JP.pixelAt image x ((JP.imageHeight image) - 1 - y)

-- toWordVec :: Colour -> SV.Vector Word8
-- toWordVec (Colour r g b) = SV.fromList [ (toEnum $ fromEnum (r * 255.0))
--                                        , (toEnum $ fromEnum (g * 255.0))
--                                        , (toEnum $ fromEnum (b * 255.0))
--                                        ]
