module Tracy.Objects.Triangle
  ( tri
  , triWithNormals
  )
  where

import Control.Applicative
import Control.Lens
import Linear

import Tracy.Types
import Tracy.Util
import Tracy.Constants

tri :: V3 Float -> V3 Float -> V3 Float -> Material -> Object
tri v0 v1 v2 mat =
    let n = signorm $ cross (v1 - v0) (v2 - v0)
    in triWithNormals v0 v1 v2 n n n mat

triWithNormals :: V3 Float -> V3 Float -> V3 Float -> V3 Float -> V3 Float -> V3 Float -> Material -> Object
triWithNormals v0 v1 v2 n0 n1 n2 mat =
    Object { _objectMaterial = mat
           , _hit = hitTriangle v0 v1 v2 n0 n1 n2 mat
           , _shadow_hit = shadowHitTriangle v0 v1 v2
           , _bounding_box = Just $ triBBox v0 v1 v2
           , _areaLightImpl = Nothing
           }

interpolateNormal :: V3 Float -> V3 Float -> V3 Float -> Float -> Float -> V3 Float
interpolateNormal n0 n1 n2 beta tgamma =
    signorm $ (1 - beta - tgamma) *^ n0 +
              beta *^ n1 +
              tgamma *^ n2

triBBox :: V3 Float -> V3 Float -> V3 Float -> BBox
triBBox v0 v1 v2 =
    BBox (V3 minx miny minz) (V3 maxx maxy maxz)
    where
      delta = 0.0001
      minx = min3 (v0^._x) (v1^._x) (v2^._x) - delta
      miny = min3 (v0^._y) (v1^._y) (v2^._y) - delta
      minz = min3 (v0^._z) (v1^._z) (v2^._z) - delta

      maxx = max3 (v0^._x) (v1^._x) (v2^._x) + delta
      maxy = max3 (v0^._y) (v1^._y) (v2^._y) + delta
      maxz = max3 (v0^._z) (v1^._z) (v2^._z) + delta

_hitTriangle :: V3 Float -> V3 Float -> V3 Float
             -> V3 Float -> V3 Float -> V3 Float
             -> Ray -> Maybe (Float, V3 Float)
_hitTriangle v0 v1 v2 n0 n1 n2 ray =
        if beta < 0 || tgamma < 0 || beta + tgamma > 1 || t < epsilon
           then Nothing
           else Just (t, theNormal)
    where
        t = e3 * inv_denom
        e3 = a * p - b * r + d * s
        tgamma = e2 * inv_denom
        e2 = a * n + d * q + c * r
        r = e * l - h * i
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
        theNormal = interpolateNormal n0 n1 n2 beta tgamma

hitTriangle :: V3 Float -> V3 Float -> V3 Float -> V3 Float
            -> V3 Float -> V3 Float
            -> Material -> Ray -> Maybe (Shade, Float)
hitTriangle v0 v1 v2 n0 n1 n2 m ray =
    let mkShade t n = defaultShade { _localHitPoint = ray^.origin + (t *^ ray^.direction)
                                   , _normal = n
                                   , _material = m
                                   }
    in case _hitTriangle v0 v1 v2 n0 n1 n2 ray of
          Nothing -> Nothing
          Just (t, n) -> Just (mkShade t n, t)

shadowHitTriangle :: V3 Float -> V3 Float -> V3 Float -> Ray -> Maybe Float
shadowHitTriangle v0 v1 v2 r = fst <$> _hitTriangle v0 v1 v2 undefined undefined undefined r
