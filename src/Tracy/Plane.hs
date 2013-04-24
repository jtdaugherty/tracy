module Tracy.Plane where

import Control.Lens
import Linear

import Tracy.Types
import Tracy.Constants
import Tracy.Util

plane :: V3 Double -> V3 Double -> Material -> Object
plane o n m =
    Object { _objectMaterial = m
           , _hit = hitPlane o n m
           }

hitPlane :: V3 Double -> V3 Double -> Material -> Ray -> Maybe (Shade, Double)
hitPlane o n m ray =
    let t = ((o - (ray^.origin)) `dot` n) / ((ray^.direction) `dot` n)
        hp = ray^.origin + (t *^ ray^.direction)
        s = defaultShade { _localHitPoint = hp
                         , _normal = n
                         , _material = m
                         }
    in if t <= epsilon
       then Nothing
       else Just (s, t)
