module Main where

import Criterion.Main
import Control.DeepSeq (NFData)
import System.Random.MWC

import Tracy.Types
import Tracy.Samplers

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
                ]
