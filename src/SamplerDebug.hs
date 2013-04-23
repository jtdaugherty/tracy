module Main where

import Control.Applicative
import Control.Lens
import Control.Monad.State
import Codec.BMP
import System.Environment
import System.Exit
import System.Random
import qualified Data.Map as M
import Data.Colour
import qualified Data.ByteString as B

import Tracy.Types
import Tracy.Util
import Tracy.Samplers

usage :: IO a
usage = do
  pn <- getProgName
  let header = "Usage: " ++ pn ++ " <sample root>"
  putStrLn header
  exitFailure

samplers :: [(String, FilePath, Sampler (Double, Double), Bool)]
samplers = [ ("regular", "regular.bmp", regular, False)
           , ("jittered", "jittered.bmp", jittered, False)
           , ("pureRandom", "pure_random.bmp", pureRandom, False)
           , ("regular/disk", "regular_disk.bmp", toUnitDisk regular, True)
           , ("jittered/disk", "jittered_disk.bmp", toUnitDisk jittered, True)
           , ("pureRandom/disk", "pure_random_disk.bmp", toUnitDisk pureRandom, True)
           ]

main :: IO ()
main = do
  args <- getArgs
  aaRoot <- case args of
              [aaRootStr] -> return (read aaRootStr :: Double)
              _ -> usage


  g <- getStdGen
  let cfg = Config { showLog = False
                   , silent = True
                   , vpSampler = const $ const $ return []
                   , sampleRoot = aaRoot
                   }
      st = TraceState { _traceLog = []
                      , _traceRNG = g
                      , _traceConfig = cfg
                      , _traceNumSampleSets = 1
                      }

  -- Create an image whose side length is 50 times the aaRoot.  This
  -- gives us better precision in locating the random values and
  -- identifying good distribution.
  let sideLen = 100.0
      blank = Colour 0 0 0
      hitColor = Colour 1 0 0

  forM_ samplers $ \(name, path, s, isDisk) -> do
         putStrLn $ "Sampling " ++ show name ++ " to " ++ path
         let vals = (runState (s aaRoot 1) st)^._1.to concat

             offsetVals = if isDisk
                          then (mapped.both %~ ((*0.5). (+1.0))) vals
                          else vals

             newVals = (mapped.both %~ (fromEnum . (*sideLen))) offsetVals
             m = M.fromList $ zip newVals $ repeat True
             img = [ if M.lookup (x, y) m == Just True then hitColor else blank
                     | x <- [0..(fromEnum sideLen)]
                     , y <- [0..(fromEnum sideLen)]
                   ]

         putStrLn "  Before conversion:"
         forM_ vals $ \v -> putStrLn $ "    " ++ show v

         putStrLn "  After conversion:"
         forM_ newVals $ \v -> putStrLn $ "    " ++ show v

         writeBMP path $ packRGBA32ToBMP
                      (fromEnum $ sideLen + 1)
                      (fromEnum $ sideLen + 1) $
                      B.concat $ getColorBytes <$> img
