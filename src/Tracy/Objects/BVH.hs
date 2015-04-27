{-# LANGUAGE BangPatterns #-}
module Tracy.Objects.BVH
  ( bvh
  , buildBVH
  , BVH(..)
  , bvhWithMaterial
  , bboxArea
  )
where

import Control.Applicative
import Control.Lens
import Data.List
import qualified Data.Map as M
import qualified Data.Vector as V
import Linear

import Tracy.Types
import Tracy.BoundingBox

data BVH = Leaf !BBox !Object
         | Node !BBox !BVH !BVH

instance Show BVH where
    show (Leaf b _) = "Leaf " ++ show b
    show (Node b n1 n2) = "Node " ++ show b ++ " (" ++ show n1 ++ ") (" ++ show n2 ++ ")"

bvh :: [Object] -> Object
bvh os = bvh_ os Nothing

bvhWithMaterial :: [Object] -> Material -> Object
bvhWithMaterial os m = bvh_ os (Just m)

bvh_ :: [Object] -> Maybe Material -> Object
bvh_ os mat =
    let theBvh = buildBVH os
    in Object { _objectMaterial = case mat of
                                    Nothing -> error "should not use objectMaterial of BVH"
                                    Just m -> m
              , _hit = hitF theBvh
              , _shadow_hit = (snd <$>) . hitF theBvh
              , _bounding_box = Just $ bvhBbox theBvh
              , _areaLightImpl = Nothing
              }

hitF :: BVH -> Ray -> Maybe (Shade, Double)
hitF (Leaf b o) r =
    boundingBoxHit b r >> (o^.hit) r
hitF (Node b o1 o2) r =
    boundingBoxHit b r >>
      case (hitF o1 r, hitF o2 r) of
        (Just (sh1, d1), Just (sh2, d2)) ->
            if d1 < d2 then Just (sh1, d1) else Just (sh2, d2)
        (Nothing, Nothing) -> Nothing
        (Just v, _) -> Just v
        (_, Just v) -> Just v

buildBVH :: [Object] -> BVH
buildBVH os = buildBVH_ $ M.fromList $ zip [1..] $ mkSingleton <$> os
    where
        mkSingleton o = Leaf (maybe errMsg id (o^.bounding_box)) o
        errMsg = error "BVH: cannot add an object that does not provide a bounding box"

buildBVH_ :: M.Map Int BVH -> BVH
buildBVH_ m
  | M.null m = error "BUG: buildBVH_ should never get the empty list"
  | M.size m == 1 = snd $ head $ M.toList m
  | otherwise = buildBVH_ newMap
    where
        allPairs = pairs $ M.keys m
        dists = (\(ia, ib) -> dist (m M.! ia) (m M.! ib)) <$> allPairs
        sorted = sort $ zip (fst <$> dists) $ zip (snd <$> dists) allPairs
        (_, (bestBbox, (bestA_ix, bestB_ix))) = head sorted
        newNode = Node bestBbox (m M.! bestA_ix) (m M.! bestB_ix)
        newMap = M.delete bestB_ix $ M.insert bestA_ix newNode m

dist :: BVH -> BVH -> (Double, BBox)
dist a b = (bboxArea newBox, newBox)
    where
        newBox = enclosingBBox $ V.fromList [bvhBbox a, bvhBbox b]

bboxArea :: BBox -> Double
bboxArea (BBox p0 p1) = 4 * (w * h + l * w + l * h)
    where
        -- center point is halfway between the corners
        c = (1/2) *^ (p0 + p1)
        -- vector from center to corner is used to calculate dimensions
        d = p0 - c
        -- w/h/d is twice the projection of d onto each axis
        h = abs $ d^._y
        w = abs $ d^._x
        l = abs $ d^._z

bvhBbox :: BVH -> BBox
bvhBbox (Leaf b _) = b
bvhBbox (Node b _ _) = b

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs [_] = []
pairs (a:as) = map (\v -> (a, v)) as ++ pairs as
