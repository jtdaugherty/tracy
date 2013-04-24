{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tracy.Main where

import Control.Applicative
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan.Strict
import Control.Lens
import Control.Monad
import Control.DeepSeq
import System.IO
import Codec.BMP
import Data.Maybe
import Data.Time.Clock
import Data.Colour
import qualified Data.Map as M
import qualified Data.ByteString as B

import Tracy.Types
import Tracy.Samplers
import Tracy.Cameras
import Tracy.Util

defaultConfig :: Config
defaultConfig =
    Config { vpSampler = regular
           , sampleRoot = 4
           , numThreads = 1
           }

batch :: Int -> [a] -> [[a]]
batch batches vals = reverse $ batch_ (length vals) batches vals []

batch_ :: Int -> Int -> [a] -> [[a]] -> [[a]]
batch_ _ _ [] r = r
batch_ tot sz xs (r:rs)
    | length xs < (tot `div` sz) = (r ++ xs) : rs
batch_ tot sz vals result =
    let (a, b) = splitAt (tot `div` sz) vals
    in batch_ tot sz b (a : result)

instance NFData Colour where
    rnf (Colour r g b) = r `seq` g `seq` b `seq` ()

render :: Config -> Camera ThinLens -> World -> FilePath -> IO ()
render cfg cam w filename = do
  putStrLn $ "Rendering " ++ filename ++ " ..."
  putStrLn $ "  Objects: " ++ (w^.objects.to length.to show)

  t1 <- getCurrentTime

  let numSets = fromEnum (w^.viewPlane.hres * 1.7)
      squareSampler = cfg^.to vpSampler
      diskSampler = cam^.cameraData.lensSampler

  -- Generate sample data for square and disk samplers
  squareSamples <- replicateM numSets $ squareSampler (cfg^.to sampleRoot)
  diskSamples <- replicateM numSets $ diskSampler (cfg^.to sampleRoot)

  let renderer = cam^.cameraRenderWorld

      workerThread ch rows =
          do
            forM_ rows $ \r ->
                do
                  let cs = renderer cam squareSamples diskSamples numSets cfg r w
                  writeChan ch (r, cs)

      processResponses ch m =
          do
            (finishedRow, rowColors) <- readChan ch
            let m' = M.insert finishedRow rowColors m
                perc :: Int
                perc = truncate $ ((100.0 * (toEnum $ M.size m')) / w^.viewPlane.vres)
            putStr $ "\r" ++ (show perc) ++ "% ... "
            hFlush stdout
            if M.size m' == (fromEnum $ w^.viewPlane.vres) then
                return m' else
                processResponses ch m'

  let rowBatches = batch (cfg^.to numThreads) [0..(fromEnum $ w^.viewPlane.vres-1)]

  dataChan <- newChan

  putStrLn $ "Worker threads: " ++ (cfg^.to numThreads.to show)

  forM_ rowBatches $ \rows ->
      do
        forkIO (workerThread dataChan rows)

  -- Wait on data from the channel, build up the row map as the
  -- responses come in (and print some output)
  rowMap <- processResponses dataChan M.empty

  -- Now sort the map contents and concatenate the results to generate
  -- the image
  let imgBytes = B.concat [ B.concat $ getColorBytes <$> (fromJust $ M.lookup r rowMap)
                            | r <- [0..(fromEnum $ w^.viewPlane.vres-1)]
                          ]
      img = packRGBA32ToBMP (fromEnum $ w^.viewPlane^.hres)
                            (fromEnum $ w^.viewPlane^.vres) imgBytes

  writeBMP filename img

  t2 <- getCurrentTime

  putStrLn $ "done. Total time: " ++ (show $ diffUTCTime t2 t1)
