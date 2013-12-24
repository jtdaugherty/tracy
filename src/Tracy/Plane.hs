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
           , _bounding_box = Nothing
           }

shadowHitPlane :: V3 Float -> V3 Float -> Ray -> Maybe Float
shadowHitPlane = _hitPlane

_hitPlane :: V3 Float -> V3 Float -> Ray -> Maybe Float
_hitPlane o n r =
    let t = ((o - (r^.origin)) `dot` n) / ((r^.direction) `dot` n)
    in if t > epsilon
       then Just t else Nothing

hitPlane :: V3 Float -> V3 Float -> Material -> Ray -> Maybe (Shade, Float)
hitPlane o n m ray =
    let mkHitPoint t = ray^.origin + (t *^ ray^.direction)
        s hp = defaultShade { _localHitPoint = hp
                            , _normal = n
                            , _material = m
                            }
    in case _hitPlane o n ray of
        Nothing -> Nothing
        Just t -> Just (s $ mkHitPoint t, t)
