module Tracy.Sphere where

import Control.Lens
import Linear

import Tracy.Types
import Tracy.Constants
import Tracy.Util

sphere :: V3 Double -> Double -> Material -> Object
sphere p rad m =
    Object { _objectMaterial = m
           , _hit = hitSphere p rad m
           }

hitSphere :: V3 Double -> Double -> Material -> Ray
          -> Maybe (Shade, Double)
hitSphere p rad mat ray =
    let temp = ray^.origin - p
        a = (ray^.direction) `dot` (ray^.direction)
        b = (2 * temp) `dot` (ray^.direction)
        c = (temp `dot` temp) - rad * rad
        disc = b * b - 4 * a * c
        e = sqrt disc
        denom = 2 * a

        t1 = (-b - e) / denom
        t2 = (-b + e) / denom

        makeShade tval = (sh, tval)
            where
              sh = defaultShade { _localHitPoint = ray^.origin + (tval *^ ray^.direction)
                                , _material = mat
                                , _normal = (temp + (tval *^ ray^.direction)) ^/ rad
                                }

    in if t1 > epsilon
       then Just $ makeShade t1
       else if t2 > epsilon
            then Just $ makeShade t2
            else Nothing
