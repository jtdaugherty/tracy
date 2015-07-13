module Tracy.DataHandlers.FileHandler
  ( fileHandler
  , writeImage
  , buildFilename
  )
  where

import Control.Applicative
import Control.Concurrent.Chan
import qualified Data.Map as M
import qualified Data.ByteString as B
import Codec.BMP
import qualified Data.Vector.Storable as SV

import Tracy.Types
import Tracy.Util

buildFilename :: String -> Frame -> FilePath
buildFilename sn (Frame fn) = sn ++ "-" ++ show fn ++ ".bmp"

fileHandler :: Chan DataEvent -> IO ()
fileHandler chan = do
  DSceneName sceneName <- readChan chan
  DSampleRoot _ <- readChan chan
  DImageSize (Width cols) (Height rows) <- readChan chan
  DRowRanges rowRanges <- readChan chan

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
              work $ M.alter (\(Just v) -> Just (v + sc)) startRow m
            DFinished frameNum -> do
                vec <- vectorFromMergeBuffer merged
                let vec2 = SV.map maxToOne vec
                writeImage vec2 cols rows (buildFilename sceneName frameNum)
                -- Reset sample count state
                work sCountMap
            DShutdown -> return ()
            DStarted _ -> work m
            _ -> error "FileHandler: unexpected event!"

  work sCountMap

writeImage :: SV.Vector Color -> Int -> Int -> FilePath -> IO ()
writeImage dat cols rows filename = do
  let imgBytes = B.concat $ (getColorBytes <$> (SV.toList dat))
      img = packRGBA32ToBMP (fromEnum cols) (fromEnum rows) imgBytes

  writeBMP filename img
