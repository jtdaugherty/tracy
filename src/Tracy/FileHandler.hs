module Tracy.FileHandler where

import Control.Applicative
import Control.Monad
import Control.Concurrent.Chan
import qualified Data.ByteString as B
import Data.List
import Codec.BMP

import Tracy.Types
import Tracy.Util

fileHandler :: FilePath -> Chan DataEvent -> IO ()
fileHandler filename chan = do
  DNumChunks chunks <- readChan chan
  DImageSize cols rows <- readChan chan

  DStarted <- readChan chan

  result <- forM [1..chunks] $ \_ -> do
      DChunkFinished ch rs <- readChan chan
      return (ch, rs)

  DFinished <- readChan chan

  let imgBytes = B.concat $ B.concat <$> (getColorBytes <$>) <$> (concat $ snd <$> sort result)
      img = packRGBA32ToBMP (fromEnum rows) (fromEnum cols) imgBytes

  writeBMP filename img

