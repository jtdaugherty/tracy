{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tracy.Main where

import Control.Applicative
import Control.Parallel.Strategies
import Control.Lens
import Control.DeepSeq
import Control.Monad
import Codec.BMP
import Data.Time.Clock
import Data.Colour
import System.IO
import qualified Data.ByteString as B
import qualified Data.Vector as V
import GHC.Conc

import Tracy.Types
import Tracy.Samplers
import Tracy.Cameras
import Tracy.Util
import Tracy.Grid

defaultConfig :: IO Config
defaultConfig = do
    n <- getNumProcessors
    return $ Config { _vpSampler = regular
                    , _sampleRoot = 4
                    , _accelScheme = gridScheme
                    , _cpuCount = n
                    , _workChunks = 10
                    }

instance NFData Colour where
    rnf (Colour r g b) = r `seq` g `seq` b `seq` ()

noScheme :: AccelScheme
noScheme = AccelScheme "none" id

accelSchemes :: [AccelScheme]
accelSchemes =
    [ noScheme
    , gridScheme
    ]

render :: Config -> Camera ThinLens -> World -> FilePath -> IO ()
render cfg cam w filename = do
  let root = cfg^.sampleRoot
  putStrLn $ "Rendering " ++ filename ++ " ..."
  putStrLn $ "  Sampler root: " ++ (root^.to show) ++ " (" ++ (root^.to (**2).to show) ++ " samples per pixel)"
  putStrLn $ "  Acceleration: " ++ (cfg^.accelScheme.schemeName)
  putStrLn $ "  Objects: " ++ (w^.objects.to length.to show)
  putStrLn $ "  Shadows: " ++ (if w^.worldShadows then "yes" else "no")

  t1 <- getCurrentTime

  let numSets = fromEnum (w^.viewPlane.hres * 2.3)
      squareSampler = cfg^.vpSampler
      diskSampler = cam^.cameraData.lensSampler

  -- Generate sample data for square and disk samplers
  squareSamples <- V.replicateM numSets $ squareSampler (cfg^.sampleRoot)
  diskSamples <- V.replicateM numSets $ diskSampler (cfg^.sampleRoot)

  putStrLn $ "  Square sample sets: " ++ (show $ V.length squareSamples)
  putStrLn $ "  Disk sample sets: " ++ (show $ V.length diskSamples)

  let renderer = cam^.cameraRenderWorld
      worker r = renderer cam squareSamples diskSamples numSets cfg r w

  putStrLn $ "  Using CPUs: " ++ show (cfg^.cpuCount)
  hFlush stdout

  let numChunks = cfg^.workChunks
      rowsPerChunk = w^.viewPlane.vres / (toEnum numChunks)
      chunk f xs = result : chunk f rest where (result, rest) = f xs
      chunks = filter (not . null) $ take (numChunks + 1) $ chunk (splitAt (fromEnum rowsPerChunk)) rows
      rows = [0..(fromEnum $ w^.viewPlane.vres-1)]

  putStrLn $ "  Chunks: " ++ (show $ length chunks)
  putStrLn $ "  Pixel rows per chunk: " ++ (show rowsPerChunk)

  putStr $ "  Rendering: 0/" ++ show (length chunks)
  hFlush stdout

  result <- forM (zip ([1..]::[Int]) chunks) $
    \(chunkId, chunkRows) -> do
        let !r = parMap (rpar `dot` rdeepseq) worker chunkRows
        r `deepseq` return ()
        putStr $ "\r  Rendering: " ++ (show chunkId) ++ "/" ++ show (length chunks)
        hFlush stdout
        return r

  let imgBytes = B.concat $ B.concat <$> (getColorBytes <$>) <$> (concat result)
      img = packRGBA32ToBMP (fromEnum $ w^.viewPlane^.hres)
                            (fromEnum $ w^.viewPlane^.vres) imgBytes
  writeBMP filename img

  t2 <- getCurrentTime

  putStrLn $ "\n  Total time: " ++ (show $ diffUTCTime t2 t1)
