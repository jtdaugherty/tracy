module Tracy.Objects.Sphere
  ( sphere
  )
  where

import Control.Applicative
import Control.Lens
import Linear

import Tracy.Types
import Tracy.Constants
import Tracy.Util
import Tracy.BoundingBox

sphere :: V3 Float -> Float -> Material -> Object
sphere p rad m =
    Object { _objectMaterial = m
           , _hit = hitSphere p rad m
           , _shadow_hit = shadowHitSphere p rad
           , _bounding_box = Just $ sphereBBox p rad
           , _areaLightImpl = Nothing
           }

sphereBBox :: V3 Float -> Float -> BBox
sphereBBox p rad = boundingBox p0 p1
    where
      delta = 0.00001
      p0 = V3 (p^._x - rad - delta) (p^._y - rad - delta) (p^._z - rad - delta)
      p1 = V3 (p^._x + rad + delta) (p^._y + rad + delta) (p^._z + rad + delta)

shadowHitSphere :: V3 Float -> Float -> Ray -> Maybe Float
shadowHitSphere = _hitSphere

_hitSphere :: V3 Float -> Float -> Ray -> Maybe Float
_hitSphere p rad ray =
    let temp = ray^.origin - p
        a = (ray^.direction) `dot` (ray^.direction)
        b = (2 * temp) `dot` (ray^.direction)
        c = (temp `dot` temp) - rad * rad
        disc = b * b - 4 * a * c
        e = sqrt disc
        denom = 2 * a

        t1 = (-b - e) / denom
        t2 = (-b + e) / denom
    in if t1 > epsilon
       then Just t1
       else if t2 > epsilon
            then Just t2
            else Nothing

hitSphere :: V3 Float -> Float -> Material -> Ray
          -> Maybe (Shade, Float)
hitSphere p rad mat ray =
    let temp = ray^.origin - p
        makeShade tval = (sh, tval)
            where
              sh = defaultShade { _localHitPoint = ray^.origin + (tval *^ ray^.direction)
                                , _material = mat
                                , _normal = (temp + (tval *^ ray^.direction)) ^/ rad
                                }
    in makeShade <$> _hitSphere p rad ray
