module Tracy.Plane where

import Control.Lens
import Linear

import Tracy.Types
import Tracy.Constants
import Tracy.Util

plane :: V3 Float -> V3 Float -> Material -> Object
plane o n m =
    Object { _objectMaterial = m
           , _hit = hitPlane o n m
           , _shadow_hit = shadowHitPlane o n
           }

shadowHitPlane :: V3 Float -> V3 Float -> Ray -> Maybe Float
shadowHitPlane o n r =
    let t = ((o - (r^.origin)) `dot` n) / ((r^.direction) `dot` n)
    in if t > epsilon
       then Just t else Nothing

hitPlane :: V3 Float -> V3 Float -> Material -> Ray -> Maybe (Shade, Float)
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
