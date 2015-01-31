module Main where

import Criterion.Main
import System.Random.MWC

import Tracy.Samplers

main :: IO ()
main = do
    gen <- createSystemRandom
    defaultMain [ bgroup "regular" [ bench "1"  $ whnfIO $ runSampler regular gen 1
                                   , bench "2"  $ whnfIO $ runSampler regular gen 2
                                   , bench "3"  $ whnfIO $ runSampler regular gen 3
                                   , bench "4"  $ whnfIO $ runSampler regular gen 4
                                   , bench "5"  $ whnfIO $ runSampler regular gen 5
                                   ]
                ] 
