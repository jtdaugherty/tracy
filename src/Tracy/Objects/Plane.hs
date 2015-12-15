{-# LANGUAGE BangPatterns #-}
module Tracy.Objects.Plane where

import Control.Lens
import Linear

import Tracy.Types
import Tracy.Constants
import Tracy.Util

plane :: Material -> Object
plane m =
    Object { _objectMaterial = m
           , _hit = hitPlane m
           , _shadow_hit = _hitPlane
           , _bounding_box = Nothing
           , _areaLightImpl = Nothing
           }

_hitPlane :: Ray -> Maybe Double
_hitPlane r =
    let t = ((o - (r^.origin)) `dot` n) / denom
        !denom = (r^.direction) `dot` n
        o = V3 0 0 0
        n = V3 0 1 0
    in if denom == 0
       then Nothing
       else if t > epsilon
            then Just t else Nothing

hitPlane :: Material -> Ray -> Maybe (Shade, Double)
hitPlane m ray =
    let mkHitPoint t = ray^.origin + (t *^ ray^.direction)
        o = V3 0 0 0
        n = V3 0 1 0
        s hp = defaultShade { _localHitPoint = hp
                            , _normal = n
                            , _material = m
                            }
    in case _hitPlane ray of
        Nothing -> Nothing
        Just t -> Just (s $ mkHitPoint t, t)
