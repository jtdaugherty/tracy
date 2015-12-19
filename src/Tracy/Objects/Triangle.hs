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

tri :: V3 Double -> V3 Double -> V3 Double -> Material -> Object
tri v0 v1 v2 mat =
    let n = signorm $ cross (v1 - v0) (v2 - v0)
    in triWithNormals (v0, n, Nothing) (v1, n, Nothing) (v2, n, Nothing) mat

triWithNormals :: (V3 Double, V3 Double, Maybe (V2 Double))
               -> (V3 Double, V3 Double, Maybe (V2 Double))
               -> (V3 Double, V3 Double, Maybe (V2 Double))
               -> Material -> Object
triWithNormals (v0, n0, uv0) (v1, n1, uv1) (v2, n2, uv2) mat =
    Object { _objectMaterial = mat
           , _hit = hitTriangle v0 v1 v2 n0 n1 n2 uv0 uv1 uv2 mat
           , _shadow_hit = shadowHitTriangle v0 v1 v2
           , _bounding_box = Just $ triBBox v0 v1 v2
           , _areaLightImpl = Nothing
           }

interpolateNormal :: V3 Double -> V3 Double -> V3 Double -> Double -> Double -> V3 Double
interpolateNormal n0 n1 n2 beta tgamma =
    signorm $ (1 - beta - tgamma) *^ n0 +
              beta *^ n1 +
              tgamma *^ n2

interpolateUV :: V2 Double -> V2 Double -> V2 Double -> Double -> Double -> V2 Double
interpolateUV uv0 uv1 uv2 beta tgamma =
    (1 - beta - tgamma) *^ uv0 +
    beta *^ uv1 +
    tgamma *^ uv2

triBBox :: V3 Double -> V3 Double -> V3 Double -> BBox
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

_hitTriangle :: V3 Double -> V3 Double -> V3 Double
             -> V3 Double -> V3 Double -> V3 Double
             -> Maybe (V2 Double) -> Maybe (V2 Double) -> Maybe (V2 Double)
             -> Ray -> Maybe (Double, V3 Double, Maybe (V2 Double))
_hitTriangle v0 v1 v2 n0 n1 n2 uv0 uv1 uv2 ray =
        if beta < 0 || tgamma < 0 || beta + tgamma > 1 || t < epsilon
           then Nothing
           else Just (t, theNormal, theUV)
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
        theUV = interpolateUV <$> uv0 <*> uv1 <*> uv2 <*> pure beta <*> pure tgamma

hitTriangle :: V3 Double -> V3 Double -> V3 Double
            -> V3 Double -> V3 Double -> V3 Double
            -> Maybe (V2 Double) -> Maybe (V2 Double) -> Maybe (V2 Double)
            -> Material -> Ray -> Maybe (Shade, Double)
hitTriangle v0 v1 v2 n0 n1 n2 uv0 uv1 uv2 m ray =
    let mkShade t n uv =
            defaultShade { _localHitPoint = ray^.origin + (t *^ ray^.direction)
                         , _normal = n
                         , _material = m
                         , _mappingU = (^._x) <$> uv
                         , _mappingV = (^._y) <$> uv
                         }
    in case _hitTriangle v0 v1 v2 n0 n1 n2 uv0 uv1 uv2 ray of
          Nothing -> Nothing
          Just (t, n, uv) -> Just (mkShade t n uv, t)

shadowHitTriangle :: V3 Double -> V3 Double -> V3 Double -> Ray -> Maybe Double
shadowHitTriangle v0 v1 v2 r =
    let fst3 (a, _, _) = a
    in fst3 <$> _hitTriangle v0 v1 v2
                  undefined undefined undefined
                  undefined undefined undefined r
