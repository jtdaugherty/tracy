{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import Criterion.Main
import Control.DeepSeq (NFData(..))
import Control.Lens
import Data.Colour
import Linear (V3(V3))
import System.Random.MWC

import Tracy.Types
import Tracy.Samplers
import Tracy.Objects.Sphere
import Tracy.Objects.BVH
import Tracy.Materials.Matte

instance NFData (V3 Double) where
    rnf (V3 a b c) = a `seq` b `seq` c `seq` ()

instance NFData Object where
    rnf o = (o^.objectMaterial) `seq`
            (o^.hit) `seq`
            (o^.shadow_hit) `seq`
            (o^.bounding_box) `seq`
            (o^.areaLightImpl) `seq`
            ()

instance NFData BVH where
    rnf (Leaf b o) = b `seq` o `seq` ()
    rnf (Node b o1 o2) = b `seq` o1 `seq` o2 `seq` ()

mkSamplerGroup :: (NFData a) => GenIO -> String -> Sampler a -> Benchmark
mkSamplerGroup gen name s =
    bgroup name [ bench "4"  $ nfIO $ runSampler s gen 4
                , bench "10" $ nfIO $ runSampler s gen 10
                , bench "20" $ nfIO $ runSampler s gen 20
                , bench "32" $ nfIO $ runSampler s gen 32
                ]

bvhObjects :: [Object]
bvhObjects =
    (flip map) [1..100] $ \i ->
        sphere (V3 i i i) 0.5 $ matteFromColor cWhite

allGroups :: GenIO -> [Benchmark]
allGroups gen =
    [ bgroup "samplers" [ mkSamplerGroup gen "regular" regular
                        , mkSamplerGroup gen "pureRandom" pureRandom
                        , mkSamplerGroup gen "jittered" jittered
                        , mkSamplerGroup gen "multiJittered" multiJittered
                        , mkSamplerGroup gen "correlatedMultiJittered" correlatedMultiJittered
                        ]
    , bench "toUnitHemi" $ nf (toUnitHemi 1) (0.5, 0.5)
    , bgroup "toUnitDisk" [ bench "center" $ nf toUnitDisk (0.5, 0.5)
                          , bench "UL" $ nf toUnitDisk (-0.2, 0.2)
                          , bench "LL" $ nf toUnitDisk (-0.2, -0.2)
                          , bench "UR" $ nf toUnitDisk (0.2, 0.2)
                          , bench "LR" $ nf toUnitDisk (0.2, -0.2)
                          ]
    , bench "buildBVH" $ nf buildBVH bvhObjects
    ]

main :: IO ()
main = do
    gen <- createSystemRandom
    defaultMain (allGroups gen)
