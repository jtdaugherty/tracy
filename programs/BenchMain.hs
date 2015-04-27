{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import Criterion.Main
import Control.DeepSeq (NFData(..))
import Control.Lens
import qualified Data.Vector as V
import Data.Colour
import Linear (V3(V3))
import System.Random.MWC

import Tracy.Types
import Tracy.Samplers
import Tracy.BoundingBox
import Tracy.Objects.Sphere
import Tracy.Objects.Box
import Tracy.Objects.BVH
import Tracy.Objects.Triangle
import Tracy.Objects.Torus
import Tracy.Objects.Rectangle
import Tracy.Objects.Plane
import Tracy.Objects.Compound
import Tracy.Objects.Grid
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

tri1 :: Object
tri1 = tri (V3 1 (-1) 0) (V3 0 1 0) (V3 (-1) (-1) 0) $ matteFromColor cWhite

torus1 :: Object
torus1 = torus 10 1 $ matteFromColor cWhite

rect1 :: Object
rect1 = rectangle (V3 (-1) (-1) 0) (V3 0 2 0) (V3 2 0 0) False $ matteFromColor cWhite

plane1 :: Object
plane1 = plane (V3 0 0 0) (V3 0 0 1) $ matteFromColor cWhite

compound1 :: Object
compound1 = compound v $ matteFromColor cWhite
    where
        v = V.fromList [box1, plane1, sphere1]

grid1 :: Object
grid1 = grid $ V.fromList spheres
    where
        rad = 1
        spheres = do
            x <- [-1, 0, 1]
            y <- [-1, 0, 1]
            return $ sphere (V3 (3 * rad * x) (3 * rad * y) 0) rad $ matteFromColor cWhite

bbox1 :: BBox
bbox1 = boundingBox (V3 1 1 1) (V3 (-1) (-1) (-1))

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
    , bgroup "boundingBox" [ bench "hit" $ nf (boundingBoxHit bbox1) (Ray (V3 0 0 10) (V3 0 0 (-1)))
                           , bench "miss" $ nf (boundingBoxHit bbox1) (Ray (V3 0 20 10) (V3 0 0 (-1)))
                           ]
    , bgroup "objects"
        [ bgroup "sphere" [ bench "hit" $ nf (sphere1^.hit) (Ray (V3 0 0 10) (V3 0 0 (-1)))
                          , bench "miss" $ nf (sphere1^.hit) (Ray (V3 0 20 10) (V3 0 0 (-1)))
                          ]
        , bgroup "box" [ bench "hit" $ nf (box1^.hit) (Ray (V3 0 0 10) (V3 0 0 (-1)))
                       , bench "miss" $ nf (box1^.hit) (Ray (V3 0 3 10) (V3 0 0 (-1)))
                       ]
        , bgroup "triangle" [ bench "hit" $ nf (tri1^.hit) (Ray (V3 0 0 10) (V3 0 0 (-1)))
                            , bench "miss" $ nf (tri1^.hit) (Ray (V3 0 10 10) (V3 0 0 (-1)))
                            ]
        , bgroup "torus" [ bench "hit" $ nf (torus1^.hit) (Ray (V3 0 0 10) (V3 0 0 (-1)))
                         , bench "miss" $ nf (torus1^.hit) (Ray (V3 0 10 10) (V3 0 0 (-1)))
                         ]
        , bgroup "rectangle" [ bench "hit" $ nf (rect1^.hit) (Ray (V3 0 0 10) (V3 0 0 (-1)))
                             , bench "miss" $ nf (rect1^.hit) (Ray (V3 0 10 10) (V3 0 0 (-1)))
                             ]
        , bgroup "plane" [ bench "hit" $ nf (plane1^.hit) (Ray (V3 0 0 10) (V3 0 0 (-1)))
                         , bench "miss" $ nf (plane1^.hit) (Ray (V3 0 0 10) (V3 0 0 1))
                         ]
        , bgroup "compound" [ bench "hit" $ nf (compound1^.hit) (Ray (V3 0 0 10) (V3 0 0 (-1)))
                            , bench "miss" $ nf (compound1^.hit) (Ray (V3 0 0 10) (V3 0 0 1))
                            ]
        , bgroup "grid" [ bench "hit" $ nf (grid1^.hit) (Ray (V3 0 0 10) (V3 0 0 (-1)))
                        , bench "miss" $ nf (grid1^.hit) (Ray (V3 0 0 10) (V3 0 0 1))
                        ]
        ]
    ]

main :: IO ()
main = do
    gen <- createSystemRandom
    defaultMain (allGroups gen)
