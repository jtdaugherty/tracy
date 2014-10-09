module Tracy.DataHandlers.FileHandler
  ( fileHandler
  )
  where

import Control.Applicative
import Control.Monad
import Control.Concurrent.Chan
import qualified Data.ByteString as B
import Data.List
import Codec.BMP
import qualified Data.Vector.Storable as SV

import Tracy.Types
import Tracy.Util

fileHandler :: FilePath -> Chan DataEvent -> IO ()
fileHandler filename chan = do
  DSceneName _ <- readChan chan
  DNumChunks chunks <- readChan chan
  DImageSize cols rows <- readChan chan

  DStarted <- readChan chan

  result <- forM [1..chunks] $ \_ -> do
      DChunkFinished ch _ rs <- readChan chan
      return (ch, rs)

  DFinished <- readChan chan
  DShutdown <- readChan chan

  let dat = SV.concat $ snd <$> sort result
      imgBytes = B.concat $ (getColorBytes <$> (SV.toList dat))
      img = packRGBA32ToBMP (fromEnum rows) (fromEnum cols) imgBytes

  writeBMP filename img

