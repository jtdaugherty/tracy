module Tracy.Sphere where

import Control.Lens
import Linear

import Tracy.Types
import Tracy.Constants

sphere :: V3 Double -> Double -> Color -> Object
sphere p rad c =
    Object { _objectColor = c
           , _hit = hitSphere p rad c
           }

hitSphere :: V3 Double -> Double -> Color -> Ray
          -> Maybe (Shade, Double)
hitSphere p rad color ray =
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
              sh = Shade { _localHitPoint = ray^.origin + (tval *^ ray^.direction)
                         , _shadeColor = color
                         , _normal = (temp + (tval *^ ray^.direction)) ^/ rad
                         }

    in if t1 > epsilon
       then Just $ makeShade t1
       else if t2 > epsilon
            then Just $ makeShade t2
            else Nothing