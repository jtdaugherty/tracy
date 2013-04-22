module Tracy.Samplers where

import Control.Monad
import Control.Monad.State
import Control.Monad.Random

import Tracy.Types

getRandomUnit :: TraceM Double
getRandomUnit = do
  st <- get
  let (v, g) = randomR (0, 1) $ _traceRNG st
  put $ st { _traceRNG = g }
  return v

pureRandom :: Sampler
pureRandom numSamples numSets =
    replicateM numSets $ do
      replicateM numSamples $ do
                           a <- getRandomUnit
                           b <- getRandomUnit
                           return (a, b)

-- for regular sampling, numSamples must be a perfect square.
regular :: Sampler
regular numSamples numSets = do
  let root = sqrt (toEnum numSamples)
      slice = 1.0 / root
      ss = [ (i*slice, j*slice) |
             i <- [0..root-1]
           , j <- [0..root-1]
           ]
  return $ replicate numSets ss

-- for jittered sampling, numSamples must be a perfect square.
jittered :: Sampler
jittered numSamples numSets =
  let root = sqrt (toEnum numSamples)
  in replicateM numSets $ do
    sampleArrs <- forM [0..root-1] $ \k ->
                  forM [0..root-1] $ \j ->
                  do
                    a <- getRandomUnit
                    b <- getRandomUnit
                    return ((k + a) / root, (j + b) / root)

    return $ concat sampleArrs
