module Tracy.Objects.Sphere
  ( sphere
  , concaveSphere
  )
  where

import Control.Lens
import Linear

import Tracy.Types
import Tracy.Constants
import Tracy.Util
import Tracy.BoundingBox

sphere :: Material -> Object
sphere m =
    Object { _objectMaterial = m
           , _hit = hitSphere m
           , _shadow_hit = shadowHitSphere
           , _bounding_box = Just sphereBBox
           , _areaLightImpl = Nothing
           }

concaveSphere :: Material -> Object
concaveSphere m = (sphere m) { _hit = concaveSphereHit m }

concaveSphereHit :: Material -> Ray -> Maybe (Shade, Double)
concaveSphereHit m r = concaveNormal <$> hitSphere m r
    where
        concaveNormal (sh, t) = (sh & normal %~ (^* (-1)), t)

sphereBBox :: BBox
sphereBBox = boundingBox p0 p1
    where
      delta = 0.00001
      rad = 1
      p = V3 0 0 0
      p0 = V3 (p^._x - rad - delta) (p^._y - rad - delta) (p^._z - rad - delta)
      p1 = V3 (p^._x + rad + delta) (p^._y + rad + delta) (p^._z + rad + delta)

shadowHitSphere :: Ray -> Maybe Double
shadowHitSphere = _hitSphere

_hitSphere :: Ray -> Maybe Double
_hitSphere ray =
    let p = V3 0 0 0
        rad = 1
        temp = ray^.origin - p
        a = (ray^.direction) `dot` (ray^.direction)
        b = (2 * temp) `dot` (ray^.direction)
        c = (temp `dot` temp) - rad * rad
        disc = b * b - 4 * a * c
        e = sqrt disc
        denom = 2 * a

        t1 = (-b - e) / denom
        t2 = (-b + e) / denom
    in if disc < 0
       then Nothing
       else if t1 > epsilon
            then Just t1
            else if t2 > epsilon
                 then Just t2
                 else Nothing

hitSphere :: Material -> Ray -> Maybe (Shade, Double)
hitSphere mat ray = makeShade <$> _hitSphere ray
    where
        makeShade tval = (sh, tval)
            where
              hp = ray^.origin + (tval *^ ray^.direction)
              theta = acos (hp^._y)
              phi' = atan2 (hp^._x) (hp^._z)
              phi = if phi' < 0
                    then phi' + (2 * pi)
                    else phi'
              u = phi * invTWOPI
              v = 1 - theta * invPI
              sh = defaultShade { _localHitPoint = hp
                                , _material = mat
                                , _normal = hp
                                , _mappingU = Just u
                                , _mappingV = Just v
                                }
