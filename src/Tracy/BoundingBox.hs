module Tracy.BoundingBox where

import Control.Lens
import Linear

import Tracy.Types
import Tracy.Constants

boundingBox :: V3 Float -> V3 Float -> BBox
boundingBox p0 p1 =
    BBox { _bboxP0 = p0
         , _bboxP1 = p1
         }

boundingBoxHit :: BBox -> Ray -> Maybe (Int, V3 Float, Float, V3 Float)
boundingBoxHit box ray =
    if t0 < t1 && t1 > epsilon
       then if t0 > epsilon
            then Just (face_in, faceNormal face_in, t0, localHP t0)
            else Just (face_out, faceNormal face_out, t1, localHP t1)
       else Nothing
    where
        localHP t = ray^.origin + (t *^ ray^.direction)
        maxT0 = maximum [tx_min, ty_min, tz_min]
        (t0, face_in) = if maxT0 == tx_min
                        then (tx_min, if a >= 0 then 0 else 3)
                        else if maxT0 == ty_min
                             then (ty_min, if b >= 0 then 1 else 4)
                             else (tz_min, if c >= 0 then 2 else 5)
        minT1 = minimum [tx_max, ty_max, tz_max]
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

faceNormal :: Int -> V3 Float
faceNormal 0 = V3 (-1) 0 0
faceNormal 1 = V3 0 (-1) 0
faceNormal 2 = V3 0 0 (-1)
faceNormal 3 = V3 1 0 0
faceNormal 4 = V3 0 1 0
faceNormal 5 = V3 0 0 1
faceNormal f = error $ "faceNormal: invalid face " ++ show f

inside :: BBox -> V3 Float -> Bool
inside b p =
    and [ p^._x > b^.bboxP0._x
        , p^._x < b^.bboxP1._x
        , p^._y > b^.bboxP0._y
        , p^._y < b^.bboxP1._y
        , p^._z > b^.bboxP0._z
        , p^._z < b^.bboxP1._z
        ]
