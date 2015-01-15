module Main where

import Control.Applicative
import Control.Lens
import Control.Monad.State
import Codec.BMP
import System.Environment
import System.Exit
import qualified Data.Map as M
import Data.Colour
import qualified Data.ByteString as B
import System.Random.MWC

import Tracy.Types
import Tracy.Util
import Tracy.Samplers

usage :: IO a
usage = do
  pn <- getProgName
  let header = "Usage: " ++ pn ++ " <sample root>"
  putStrLn header
  exitFailure

samplers :: [(String, FilePath, Sampler (Float, Float), Bool)]
samplers = [ ("regular", "regular.bmp", regular, False)
           , ("jittered", "jittered.bmp", jittered, False)
           , ("multiJittered", "multi_jittered.bmp", multiJittered, False)
           , ("multiJitteredInitial", "multi_jittered_initial.bmp", multiJitteredInitial, False)
           , ("pureRandom", "pure_random.bmp", pureRandom, False)
           , ("regular/disk", "regular_disk.bmp", toUnitDisk regular, True)
           , ("jittered/disk", "jittered_disk.bmp", toUnitDisk jittered, True)
           , ("pureRandom/disk", "pure_random_disk.bmp", toUnitDisk pureRandom, True)
           ]

main :: IO ()
main = do
  args <- getArgs
  aaRoot <- case args of
              [aaRootStr] -> return (read aaRootStr :: Float)
              _ -> usage


  let sideLen = 200.0
      blank = Colour 0 0 0
      hitColor = Colour 1 0 0

  rng <- create

  forM_ samplers $ \(name, path, s, isDisk) -> do
         putStrLn $ "Sampling " ++ show name ++ " to " ++ path

         vals <- s rng aaRoot

         let offsetVals = if isDisk
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
