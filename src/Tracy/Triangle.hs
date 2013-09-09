module Tracy.Triangle where

import Control.Lens
import Linear

import Tracy.Types
import Tracy.Util
import Tracy.Constants

tri :: V3 Float -> V3 Float -> V3 Float -> Material -> Object
tri v0 v1 v2 mat =
    let n = signorm $ cross (v1 - v0) (v2 - v0)
    in Object { _objectMaterial = mat
              , _hit = hitTriangle v0 v1 v2 n mat
              , _shadow_hit = shadowHitTriangle v0 v1 v2
              }

_hitTriangle :: V3 Float -> V3 Float -> V3 Float -> Ray -> Maybe Float
_hitTriangle v0 v1 v2 ray = if beta < 0 || tgamma < 0 || beta + tgamma > 1 || t < epsilon
                            then Nothing else Just t
    where
        t = e3 * inv_denom
        e3 = a * p - b * r + d * s
        tgamma = e2 * inv_denom
        e2 = a * n + d * q + c * r
        r = e * l + h * i
        beta = e1 * inv_denom
        e1 = d * m - b * n - c * p
        inv_denom = 1.0 / (a * m + b * q + c * s)
        q = g * i - e * k
        s = e * j - f * i
        p = f * l - h * j
        n = h * k - g * l
        m = f * k - g * j
        a = v0^._x - v1^._x
        b = v0^._x - v2^._x
        c = ray^.direction._x
        d = v0^._x - ray^.origin._x
        e = v0^._y - v1^._y
        f = v0^._y - v2^._y
        g = ray^.direction._y
        h = v0^._y - ray^.origin._y
        i = v0^._z - v1^._z
        j = v0^._z - v2^._z
        k = ray^.direction._z
        l = v0^._z - ray^.origin._z

hitTriangle :: V3 Float -> V3 Float -> V3 Float -> V3 Float
            -> Material -> Ray -> Maybe (Shade, Float)
hitTriangle v0 v1 v2 n m ray =
    let mkShade t = defaultShade { _localHitPoint = ray^.origin + (t *^ ray^.direction)
                                 , _normal = n
                                 , _material = m
                                 }
    in case _hitTriangle v0 v1 v2 ray of
          Nothing -> Nothing
          Just t -> Just (mkShade t, t)

shadowHitTriangle :: V3 Float -> V3 Float -> V3 Float -> Ray -> Maybe Float
shadowHitTriangle = _hitTriangle
