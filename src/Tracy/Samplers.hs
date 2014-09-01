module Tracy.Samplers
  ( regular
  , jittered
  , toDisk
  , toHemi
  , toUnitDisk
  , toUnitHemisphere
  , pureRandom
  )
  where

import Control.Applicative
import Control.Monad
import Control.Monad.Random
import Linear

import Tracy.Types

getRandomUnit :: IO Float
getRandomUnit = randomRIO (0, 1)

pureRandom :: Sampler (Float, Float)
pureRandom root =
    replicateM (fromEnum $ root * root) $ do
      a <- getRandomUnit
      b <- getRandomUnit
      return (a, b)

regular :: Sampler (Float, Float)
regular root = do
  let slice = 1.0 / root
      ss = [ ((i+0.5)*slice, (j+0.5)*slice) |
             i <- [0..root-1]
           , j <- [0..root-1]
           ]
  return ss

jittered :: Sampler (Float, Float)
jittered root =do
  sampleArrs <- forM [0..root-1] $ \k ->
                forM [0..root-1] $ \j ->
                    do
                      a <- getRandomUnit
                      b <- getRandomUnit
                      return ((k + a) / root, (j + b) / root)

  return $ concat sampleArrs

toDisk :: (Float, Float) -> (Float, Float)
toDisk (x, y) =
    let spx = 2.0 * x - 1.0
        spy = 2.0 * y - 1.0
        (r, phi) = if spx > -spy
                   then if spx > spy
                        then ( spx, spy / spx)
                        else ( spy, 2 - (spx / spy) )
                   else if spx < spy
                        then ( -spx, 4 + spy / spx )
                        else ( -spy, if spy /= 0 then 6 - (spx / spy) else 0 )
        phi' = phi * (pi / 4.0)
    in (r * cos phi', r * sin phi')

toHemi :: (Float, Float) -> V3 Float
toHemi (x, y) =
    let pu = sin_theta * cos_phi
        pv = sin_theta * sin_phi
        pw = cos_theta

        cos_phi = cos $ 2.0 * pi * x
        sin_phi = sin $ 2.0 * pi * x
        cos_theta = (1.0 - y) ** (1.0 / (exp 1.0 + 1.0))
        sin_theta = sqrt $ 1.0 - cos_theta * cos_theta

    in V3 pu pv pw

transSample :: (a -> b) -> Sampler a -> Sampler b
transSample f s root = do
  vs <- s root
  return $ f <$> vs

toUnitDisk :: Sampler (Float, Float) -> Sampler (Float, Float)
toUnitDisk = transSample toDisk

toUnitHemisphere :: Sampler (Float, Float) -> Sampler (V3 Float)
toUnitHemisphere = transSample toHemi
