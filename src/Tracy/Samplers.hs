{-# LANGUAGE BangPatterns #-}
module Tracy.Samplers
  ( regular
  , jittered
  , toUnitDisk
  , toUnitHemi
  , pureRandom
  , multiJittered
  , correlatedMultiJittered
  , multiJitteredInitial
  , runSampler
  )
  where

import Control.Applicative
import Control.Monad
import Data.Array.IO
import Data.List (transpose)
import System.Random.MWC
import Linear hiding (transpose)
import qualified Data.Vector as V

import Tracy.Types

offset :: Double
offset = 2 ** (-33)

runSampler :: Sampler a -> GenIO -> Double -> IO (V.Vector a)
runSampler (Sampler f) gen root = f gen root

getRandomUnit :: GenIO -> IO Double
getRandomUnit gen = do
    v <- uniformR (0, 1) gen
    return $ v - offset

pureRandom :: Sampler (Double, Double)
pureRandom = Sampler $ \gen root -> uniformVector gen (fromEnum $ root * root)

regular :: Sampler (Double, Double)
regular = Sampler $ \_ root -> do
  let slice = 1.0 / root
      ss = [ ((i+0.5)*slice, (j+0.5)*slice) |
             i <- [0..root-1]
           , j <- [0..root-1]
           ]
  return $ V.fromList ss

jittered :: Sampler (Double, Double)
jittered = Sampler $ \gen root -> do
  vs <- uniformVector gen (fromEnum $ root * root)

  let sampleArrs = do
        j <- [0..root-1]
        k <- [0..root-1]
        let !t = vs V.! (fromEnum $ k * root + j)
        return ( (k + fst t) / root
               , (j + snd t) / root
               )

  return $ V.fromList sampleArrs

multiJitteredBase :: GenIO -> Double -> IO [[(Double, Double)]]
multiJitteredBase gen root = do
  let r2 = root * root
  sampleArrs <- forM (zip [0..root-1] [root-1,root-2..0]) $ \(bigRow, littleCol) ->
                forM (zip [0..root-1] [root-1,root-2..0]) $ \(bigCol, littleRow) ->
                    do
                      a <- getRandomUnit gen
                      b <- getRandomUnit gen
                      return ( (bigRow/root) + (littleRow + a) / r2
                             , (bigCol/root) + (littleCol + b) / r2
                             )

  return sampleArrs

multiJitteredInitial :: Sampler (Double, Double)
multiJitteredInitial = Sampler $ \gen root -> V.fromList <$>
                       concat <$> multiJitteredBase gen root

shuffle :: GenIO -> [a] -> IO [a]
shuffle gen xs = do
        let n = length xs
        ar <- mkNewArray n xs
        forM [1..n] $ \i -> do
            j <- uniformR (i,n) gen
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    mkNewArray :: Int -> [a] -> IO (IOArray Int a)
    mkNewArray n as = newListArray (1,n) as

shuffleY :: GenIO -> Maybe [Int] -> [(Double, Double)] -> IO [(Double, Double)]
shuffleY gen mIdxs vals = do
    idxs <- case mIdxs of
              Nothing -> shuffle gen [0..length vals - 1]
              Just is -> return is
    return [ (fst $ vals !! idx, vx) | (idx, (_, vx)) <- zip idxs vals ]

shuffleX :: GenIO -> Maybe [Int] -> [(Double, Double)] -> IO [(Double, Double)]
shuffleX gen mIdxs vals = do
    idxs <- case mIdxs of
              Nothing -> shuffle gen [0..length vals - 1]
              Just is -> return is
    return [ (vy, snd $ vals !! idx) | (idx, (vy, _)) <- zip idxs vals ]

multiJittered :: Sampler (Double, Double)
multiJittered = Sampler $ \gen root -> do
  samples <- multiJitteredBase gen root

  yShuffled <- forM samples (shuffleY gen Nothing)
  xShuffled <- transpose <$> forM (transpose yShuffled) (shuffleX gen Nothing)

  return $ V.fromList $ concat xShuffled

correlatedMultiJittered :: Sampler (Double, Double)
correlatedMultiJittered = Sampler $ \gen root -> do
  samples <- multiJitteredBase gen root

  xIdxs <- shuffle gen [0..(round root)-1]
  yIdxs <- shuffle gen [0..(round root)-1]

  yShuffled <- forM samples (shuffleY gen (Just yIdxs))
  xShuffled <- transpose <$> forM (transpose yShuffled) (shuffleX gen (Just xIdxs))

  return $ V.fromList $ concat xShuffled

toUnitDisk :: (Double, Double) -> (Double, Double)
toUnitDisk (x, y) =
    let spx = 2.0 * x - 1.0
        spy = 2.0 * y - 1.0
        !(r, phi) = if spx > -spy
                    then if spx > spy
                         then ( spx, spy / spx)
                         else ( spy, 2 - (spx / spy) )
                    else if spx < spy
                         then ( -spx, 4 + spy / spx )
                         else ( -spy, if spy /= 0 then 6 - (spx / spy) else 0 )
        !phi' = phi * (pi / 4.0)
    in (r * cos phi', r * sin phi')

toUnitHemi :: Double -> (Double, Double) -> V3 Double
toUnitHemi e (x, y) =
    let pu = sin_theta * cos_phi
        pv = sin_theta * sin_phi
        pw = cos_theta

        cos_phi = cos $ 2.0 * pi * x
        sin_phi = sin $ 2.0 * pi * x
        cos_theta = (1.0 - y) ** (1.0 / (e + 1.0))
        sin_theta = sqrt $ 1.0 - cos_theta * cos_theta

    in V3 pu pv pw
