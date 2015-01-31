{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import Criterion.Main
import Control.DeepSeq (NFData(..))
import Linear (V3(V3))
import System.Random.MWC

import Tracy.Types
import Tracy.Samplers

instance NFData (V3 Double) where
    rnf (V3 a b c) = a `seq` b `seq` c `seq` ()

mkSamplerGroup :: (NFData a) => GenIO -> String -> Sampler a -> Benchmark
mkSamplerGroup gen name s =
    bgroup name [ bench "4"  $ nfIO $ runSampler s gen 4
                , bench "10" $ nfIO $ runSampler s gen 10
                , bench "20" $ nfIO $ runSampler s gen 20
                , bench "32" $ nfIO $ runSampler s gen 32
                ]

main :: IO ()
main = do
    gen <- createSystemRandom
    defaultMain [ mkSamplerGroup gen "regular" regular
                , mkSamplerGroup gen "pureRandom" pureRandom
                , mkSamplerGroup gen "jittered" jittered
                , mkSamplerGroup gen "multiJittered" multiJittered
                , mkSamplerGroup gen "correlatedMultiJittered" correlatedMultiJittered
                , bench "toUnitHemi" $ nf (toUnitHemi 1) (0.5, 0.5)
                , bench "toUnitDiskCenter" $ nf toUnitDisk (0.5, 0.5)
                , bench "toUnitDiskUL" $ nf toUnitDisk (-0.2, 0.2)
                , bench "toUnitDiskLL" $ nf toUnitDisk (-0.2, -0.2)
                , bench "toUnitDiskUR" $ nf toUnitDisk (0.2, 0.2)
                , bench "toUnitDiskLR" $ nf toUnitDisk (0.2, -0.2)
                ]
