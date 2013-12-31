{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tracy.Main where

import Control.Applicative
import Control.Parallel.Strategies
import Control.Lens
import Control.DeepSeq
import Codec.BMP
import Data.Time.Clock
import Data.Colour
import Data.Maybe
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
    return $ Config { vpSampler = regular
                    , sampleRoot = 4
                    , accelScheme = AccelGrid
                    , cpuCount = n
                    }

instance NFData Colour where
    rnf (Colour r g b) = r `seq` g `seq` b `seq` ()

showAccel :: AccelScheme -> String
showAccel AccelNone = "none"
showAccel AccelGrid = "grid"

applyAccelScheme :: AccelScheme -> World -> World
applyAccelScheme AccelNone w = w
applyAccelScheme AccelGrid w =
    let gObjs = [o | o <- _objects w, isJust $ o^.bounding_box ]
        objs = [o | o <- _objects w, not $ isJust $ o^.bounding_box ]
    in w { _objects = grid gObjs:objs }

render :: Config -> Camera ThinLens -> World -> FilePath -> IO ()
render cfg cam w filename = do
  let root = cfg^.to sampleRoot
  putStrLn $ "Rendering " ++ filename ++ " ..."
  putStrLn $ "  Sampler root: " ++ (root^.to show) ++ " (" ++ (root^.to (**2).to show) ++ " samples per pixel)"
  putStrLn $ "  Acceleration: " ++ (showAccel $ cfg^.to accelScheme)
  putStrLn $ "  Objects: " ++ (w^.objects.to length.to show)
  putStrLn $ "  Shadows: " ++ (if w^.worldShadows then "yes" else "no")

  t1 <- getCurrentTime

  let numSets = fromEnum (w^.viewPlane.hres * 2.3)
      squareSampler = cfg^.to vpSampler
      diskSampler = cam^.cameraData.lensSampler

  -- Generate sample data for square and disk samplers
  squareSamples <- V.replicateM numSets $ squareSampler (cfg^.to sampleRoot)
  diskSamples <- V.replicateM numSets $ diskSampler (cfg^.to sampleRoot)

  putStrLn $ "  Square sample sets: " ++ (show $ V.length squareSamples)
  putStrLn $ "  Disk sample sets: " ++ (show $ V.length diskSamples)

  let renderer = cam^.cameraRenderWorld
      worker r = renderer cam squareSamples diskSamples numSets cfg r w

  putStrLn $ "  Using CPUs: " ++ show (cfg^.to cpuCount)
  putStr "  Rendering ... "
  hFlush stdout

  let rows = [0..(fromEnum $ w^.viewPlane.vres-1)]
      vs = parMap (rpar `dot` rdeepseq) worker rows
      imgBytes = B.concat $ B.concat <$> (getColorBytes <$>) <$> vs
      img = packRGBA32ToBMP (fromEnum $ w^.viewPlane^.hres)
                            (fromEnum $ w^.viewPlane^.vres) imgBytes

  writeBMP filename img

  t2 <- getCurrentTime

  putStrLn $ "done.\n  Total time: " ++ (show $ diffUTCTime t2 t1)
