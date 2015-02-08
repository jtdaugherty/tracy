module Tracy.DataHandlers.FileHandler
  ( fileHandler
  , writeImage
  , buildFilename
  )
  where

import Control.Applicative
import Control.Concurrent.Chan
import Data.IORef
import qualified Data.Map as M
import qualified Data.ByteString as B
import Codec.BMP
import qualified Data.Vector.Storable as SV

import Tracy.Types
import Tracy.Util

buildFilename :: String -> Frame -> FilePath
buildFilename sn (Frame fn) = sn ++ "-" ++ show fn ++ ".bmp"

fileHandler :: FilePath -> Chan DataEvent -> IO ()
fileHandler filename chan = do
  DSceneName _ <- readChan chan
  DFrameNum _ <- readChan chan
  DSampleRoot _ <- readChan chan
  DImageSize cols rows <- readChan chan
  DRowRanges rowRanges <- readChan chan

  DStarted <- readChan chan

  merged <- createMergeBuffer rows cols

  ref <- newIORef $ M.fromList $ zip (fst <$> rowRanges) $ repeat 0

  let work = do
      ev <- readChan chan
      case ev of
          DChunkFinished rowRange rs -> do
            let Row startRow = fst rowRange
            m <- readIORef ref
            mergeChunks (m M.! startRow) startRow merged rs
            writeIORef ref $ M.alter (\(Just v) -> Just (v + 1)) startRow m
            work
          DFinished -> return ()
          _ -> error "FileHandler: unexpected event!"

  work

  DShutdown <- readChan chan

  vec <- vectorFromMergeBuffer merged
  writeImage vec rows cols filename

writeImage :: SV.Vector Color -> Int -> Int -> FilePath -> IO ()
writeImage dat rows cols filename = do
  let imgBytes = B.concat $ (getColorBytes <$> (SV.toList dat))
      img = packRGBA32ToBMP (fromEnum rows) (fromEnum cols) imgBytes

  writeBMP filename img
