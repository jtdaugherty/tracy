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

  DStarted <- readChan chan

  merged <- createMergeBuffer rows cols

  let sCountMap = M.fromList $ zip (fst <$> rowRanges) $ repeat 0
      work m = do
        ev <- readChan chan
        case ev of
            DChunkFinished rowRange rs -> do
              let startRow = fst rowRange
              mergeChunks (m M.! startRow) startRow merged rs
              work $ M.alter (\(Just v) -> Just (v + 1)) startRow m
            DFinished frameNum -> do
                vec <- vectorFromMergeBuffer merged
                writeImage vec rows cols (buildFilename sceneName frameNum)
                -- Reset sample count state
                work sCountMap
            DShutdown -> return ()
            _ -> error "FileHandler: unexpected event!"

  work sCountMap

writeImage :: SV.Vector Color -> Int -> Int -> FilePath -> IO ()
writeImage dat rows cols filename = do
  let imgBytes = B.concat $ (getColorBytes <$> (SV.toList dat))
      img = packRGBA32ToBMP (fromEnum rows) (fromEnum cols) imgBytes

  writeBMP filename img
