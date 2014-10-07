module Tracy.Objects.Rectangle
  ( rectangle
  )
  where

import Control.Applicative
import Control.Lens
import Linear

import Tracy.Types
import Tracy.Util
import Tracy.Constants

data Rect =
    Rect { rectP0 :: V3 Float
         , rectA :: V3 Float
         , rectB :: V3 Float
         , rectMaterial :: Material
         , rectNormal :: V3 Float
         , rectALen2 :: Float
         , rectBLen2 :: Float
         , rectInvArea :: Float
         }

rectangle :: V3 Float -> V3 Float -> V3 Float -> Material -> Object
rectangle p a b mat =
    let area = norm a * norm b
        r = Rect { rectP0 = p
                 , rectA = a
                 , rectB = b
                 , rectMaterial = mat
                 , rectNormal = signorm $ a `cross` b
                 , rectALen2 = norm a ** 2
                 , rectBLen2 = norm b ** 2
                 , rectInvArea = 1 / area
                 }
    in Object { _objectMaterial = mat
              , _hit = rectHit r
              , _shadow_hit = (snd <$>) . (rectHit r)
              , _bounding_box = Just $ rectBoundingBox r
              , _areaLightImpl = Just $ rectAreaLight r
              }

rectAreaLight :: Rect -> ObjectAreaLightImpl
rectAreaLight rect =
    ObjectALI { _objectSurfaceSample = rectSurfaceSample rect
              , _objectGetNormal = rectGetNormal rect
              , _objectPDF = rectPDF rect
              }

rectSurfaceSample :: Rect -> TraceM (V3 Float)
rectSurfaceSample rect = do
    sample_point <- view tdObjectSurfaceSample
    return (rectP0 rect + (sample_point^._x *^ (rectA rect)) +
                          (sample_point^._y *^ (rectB rect)))

rectGetNormal :: Rect -> V3 Float -> V3 Float
rectGetNormal rect = const $ rectNormal rect

rectPDF :: Rect -> Shade -> Float
rectPDF rect = const $ rectInvArea rect

rectBoundingBox :: Rect -> BBox
rectBoundingBox rect = BBox v0 v1
  where
    p0 = rectP0 rect
    a = rectA rect
    b = rectB rect
    delta = 0.0001
    v0 = V3 ((min (p0^._x) (p0^._x + a^._x + b^._x)) - delta)
            ((min (p0^._y) (p0^._y + a^._y + b^._y)) - delta)
            ((min (p0^._z) (p0^._z + a^._z + b^._z)) - delta)
    v1 = V3 ((max (p0^._x) (p0^._x + a^._x + b^._x)) + delta)
            ((max (p0^._y) (p0^._y + a^._y + b^._y)) + delta)
            ((max (p0^._z) (p0^._z + a^._z + b^._z)) + delta)

rectHit :: Rect -> Ray -> Maybe (Shade, Float)
rectHit rect ray =
    let t = ((rectP0 rect - ray^.origin) `dot` (rectNormal rect)) /
            ((ray^.direction) `dot` (rectNormal rect))
        p = ray^.origin + (t *^ ray^.direction)
        d = p - rectP0 rect
        ddota = d `dot` (rectA rect)
        ddotb = d `dot` (rectB rect)
        sh = defaultShade { _localHitPoint = p
                          , _normal = rectNormal rect
                          , _material = rectMaterial rect
                          }
    in if t <= epsilon ||
          (ddota < 0 || ddota > rectALen2 rect) ||
          (ddotb < 0 || ddotb > rectBLen2 rect)
       then Nothing
       else Just (sh, t)
