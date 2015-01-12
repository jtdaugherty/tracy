module Tracy.DataHandlers.FileHandler
  ( fileHandler
  , writeImage
  , buildFilename
  )
  where

import Control.Applicative
import Control.Monad
import Control.Concurrent.Chan
import qualified Data.ByteString as B
import Codec.BMP
import qualified Data.Vector.Storable as SV

import Tracy.Types
import Tracy.Util

buildFilename :: String -> Int -> FilePath
buildFilename sn fn = sn ++ "-" ++ show fn ++ ".bmp"

fileHandler :: FilePath -> Chan DataEvent -> IO ()
fileHandler filename chan = do
  DSceneName _ <- readChan chan
  DFrameNum _ <- readChan chan
  DNumBatches batches <- readChan chan
  DSampleRoot _ <- readChan chan
  DImageSize cols rows <- readChan chan

  DStarted <- readChan chan

  merged <- createMergeBuffer rows cols

  forM_ [0..batches-1] $ \batchNum -> do
      DBatchFinished rs <- readChan chan
      mergeBatches batchNum merged rs

  DFinished <- readChan chan
  DShutdown <- readChan chan

  vec <- vectorFromMergeBuffer merged
  writeImage vec rows cols filename

writeImage :: SV.Vector Color -> Int -> Int -> FilePath -> IO ()
writeImage dat rows cols filename = do
  let imgBytes = B.concat $ (getColorBytes <$> (SV.toList dat))
      img = packRGBA32ToBMP (fromEnum rows) (fromEnum cols) imgBytes

  writeBMP filename img
