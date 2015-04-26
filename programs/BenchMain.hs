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
import Tracy.Objects.Box
import Tracy.Objects.BVH
import Tracy.Materials.Matte

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

instance NFData Shade where
    rnf (Shade lhp n _ sh d) = lhp `seq` n `seq` sh `seq` d `seq` ()

mkSamplerGroup :: (NFData a) => GenIO -> String -> Sampler a -> Benchmark
mkSamplerGroup gen name s =
    bgroup name [ bench "4"  $ nfIO $ runSampler s gen 4
                , bench "10" $ nfIO $ runSampler s gen 10
                , bench "20" $ nfIO $ runSampler s gen 20
                , bench "32" $ nfIO $ runSampler s gen 32
                ]

bvhObjects :: Int -> [Object]
bvhObjects n =
    (flip map) [1..n] $ \i ->
        let v = toEnum i
        in sphere (V3 v v v) 0.5 $ matteFromColor cWhite

sphere1 :: Object
sphere1 = sphere (V3 0 0 0) 10 $ matteFromColor cWhite

box1 :: Object
box1 = box (V3 1 1 1) (V3 (-1) (-1) (-1)) $ matteFromColor cWhite

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
    , bgroup "buildBVH" [ bench "1" $ nf buildBVH (bvhObjects 1)
                        , bench "10" $ nf buildBVH (bvhObjects 10)
                        , bench "100" $ nf buildBVH (bvhObjects 100)
                        , bench "150" $ nf buildBVH (bvhObjects 150)
                        ]
    , bgroup "objects"
        [ bgroup "sphere" [ bench "hit" $ nf (sphere1^.hit) (Ray (V3 10 0 0) (V3 (-1) 0 0))
                          , bench "miss" $ nf (sphere1^.hit) (Ray (V3 10 20 0) (V3 (-1) 0 0))
                          ]
        , bgroup "box" [ bench "hit" $ nf (box1^.hit) (Ray (V3 10 0 0) (V3 (-1) 0 0))
                       , bench "miss" $ nf (box1^.hit) (Ray (V3 10 3 0) (V3 (-1) 0 0))
                       ]
        ]
    ]

main :: IO ()
main = do
    gen <- createSystemRandom
    defaultMain (allGroups gen)
