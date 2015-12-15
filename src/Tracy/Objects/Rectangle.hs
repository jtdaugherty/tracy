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
    Rect { rectP0 :: V3 Double
         , rectA :: V3 Double
         , rectB :: V3 Double
         , rectMaterial :: Material
         , rectNormal :: V3 Double
         , rectALen2 :: Double
         , rectBLen2 :: Double
         , rectInvArea :: Double
         , rectDoubleSided :: Bool
         }

rectangle :: Bool -> Material -> Object
rectangle dbl mat =
    let area = norm a * norm b
        p = V3 (-1) 0 (-1)
        a = V3 0 0 2
        b = V3 2 0 0
        r = Rect { rectP0 = p
                 , rectA = a
                 , rectB = b
                 , rectMaterial = mat
                 , rectNormal = signorm $ a `cross` b
                 , rectALen2 = norm a ** 2
                 , rectBLen2 = norm b ** 2
                 , rectInvArea = 1 / area
                 , rectDoubleSided = dbl
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

rectSurfaceSample :: Rect -> TraceM (V3 Double)
rectSurfaceSample rect = do
    sample_point <- view tdObjectSurfaceSample
    return (rectP0 rect + (sample_point^._x *^ (rectA rect)) +
                          (sample_point^._y *^ (rectB rect)))

rectGetNormal :: Rect -> Shade -> V3 Double -> V3 Double
rectGetNormal rect sh =
    const $ if rectDoubleSided rect
            then if (sh^.normal) `dot` (rectNormal rect) < 0
                 then -1 *^ (rectNormal rect)
                 else rectNormal rect
            else rectNormal rect

rectPDF :: Rect -> LightDir -> Shade -> Double
rectPDF rect = const $ const $ rectInvArea rect

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

rectHit :: Rect -> Ray -> Maybe (Shade, Double)
rectHit rect ray =
    let t = ((rectP0 rect - ray^.origin) `dot` newNormal) /
            ((ray^.direction) `dot` newNormal)
        p = ray^.origin + (t *^ ray^.direction)
        d = p - rectP0 rect
        ddota = d `dot` (rectA rect)
        ddotb = d `dot` (rectB rect)
        sh = defaultShade { _localHitPoint = p
                          , _normal = newNormal
                          , _material = rectMaterial rect
                          }
        newNormal = if rectDoubleSided rect
                    then flipNormal (ray^.direction) (rectNormal rect)
                    else rectNormal rect
    in if t <= epsilon ||
          (ddota < 0 || ddota > rectALen2 rect) ||
          (ddotb < 0 || ddotb > rectBLen2 rect)
       then Nothing
       else Just (sh, t)
