module Tracy.BoundingBox
  ( boundingBox
  , boundingBoxHit
  , inside
  , enclosingBBox
  )
  where

import Control.Lens hiding (inside)
import qualified Data.Vector as V
import Linear

import Tracy.Types
import Tracy.Constants

boundingBox :: V3 Double -> V3 Double -> BBox
boundingBox p0 p1 =
    BBox { _bboxP0 = p0
         , _bboxP1 = p1
         }

boundingBoxHit :: BBox -> Ray -> Maybe (Int, V3 Double, Double, V3 Double)
boundingBoxHit box ray =
    if t0 < t1 && t1 > epsilon
       then if t0 > epsilon
            then Just (face_in, faceNormal face_in, t0, localHP t0)
            else Just (face_out, faceNormal face_out, t1, localHP t1)
       else Nothing
    where
        localHP t = ray^.origin + (t *^ ray^.direction)

        max_1 = if tx_min > ty_min
                then tx_min else ty_min
        maxT0 = if max_1 > tz_min
                then max_1 else tz_min

        (t0, face_in) = if maxT0 == tx_min
                        then (tx_min, if a >= 0 then 0 else 3)
                        else if maxT0 == ty_min
                             then (ty_min, if b >= 0 then 1 else 4)
                             else (tz_min, if c >= 0 then 2 else 5)

        min_1 = if tx_max < ty_max
                then tx_max else ty_max
        minT1 = if tz_max < min_1
                then tz_max else min_1

        (t1, face_out) = if minT1 == tx_max
                         then (tx_max, if a >= 0 then 3 else 0)
                         else if minT1 == ty_max
                              then (ty_max, if b >= 0 then 4 else 1)
                              else (tz_max, if c >= 0 then 5 else 2)
        ox = ray^.origin._x
        oy = ray^.origin._y
        oz = ray^.origin._z
        dx = ray^.direction._x
        dy = ray^.direction._y
        dz = ray^.direction._z
        x0 = box^.bboxP0._x
        y0 = box^.bboxP0._y
        z0 = box^.bboxP0._z
        x1 = box^.bboxP1._x
        y1 = box^.bboxP1._y
        z1 = box^.bboxP1._z
        a = 1.0 / dx
        (tx_min, tx_max) = if a >= 0
                           then ((x0 - ox) * a, (x1 - ox) * a)
                           else ((x1 - ox) * a, (x0 - ox) * a)
        b = 1.0 / dy
        (ty_min, ty_max) = if b >= 0
                           then ((y0 - oy) * b, (y1 - oy) * b)
                           else ((y1 - oy) * b, (y0 - oy) * b)
        c = 1.0 / dz
        (tz_min, tz_max) = if c >= 0
                           then ((z0 - oz) * c, (z1 - oz) * c)
                           else ((z1 - oz) * c, (z0 - oz) * c)

faceNormal :: Int -> V3 Double
faceNormal 0 = V3 (-1) 0 0
faceNormal 1 = V3 0 (-1) 0
faceNormal 2 = V3 0 0 (-1)
faceNormal 3 = V3 1 0 0
faceNormal 4 = V3 0 1 0
faceNormal 5 = V3 0 0 1
faceNormal f = error $ "faceNormal: invalid face " ++ show f

inside :: BBox -> V3 Double -> Bool
inside b p =
    p^._x > b^.bboxP0._x &&
    p^._x < b^.bboxP1._x &&
    p^._y > b^.bboxP0._y &&
    p^._y < b^.bboxP1._y &&
    p^._z > b^.bboxP0._z &&
    p^._z < b^.bboxP1._z

enclosingBBox :: V.Vector BBox -> BBox
enclosingBBox boxes
  | V.null boxes = error "BUG: enclosingBBox needs at least one box"
  | otherwise = BBox newP0 newP1
    where
        corners = p0s V.++ p1s
        p0s = V.map (^.bboxP0) boxes
        p1s = V.map (^.bboxP1) boxes
        xs = V.map (^._x) corners
        ys = V.map (^._y) corners
        zs = V.map (^._z) corners
        newP0 = V3 (V.minimum xs) (V.minimum ys) (V.minimum zs)
        newP1 = V3 (V.maximum xs) (V.maximum ys) (V.maximum zs)
