{-# LANGUAGE ForeignFunctionInterface #-}
module Tracy.Objects.Torus
  ( torus
  )
  where

import Control.Applicative
import Control.Lens
import Data.List
import Data.Maybe
import Linear

import Tracy.Types
import Tracy.Util
import Tracy.BoundingBox
import Diagrams.Solve.Polynomial

torus :: Double -> Double -> Material -> Object
torus radOuter radInner m =
    let bbox = torusBBox radOuter radInner
    in Object { _objectMaterial = m
              , _hit = hitTorus radOuter radInner bbox m
              , _shadow_hit = shadowHitTorus radOuter radInner bbox
              , _bounding_box = Just bbox
              , _areaLightImpl = Nothing
              }

torusBBox :: Double -> Double -> BBox
torusBBox radOuter radInner =
    boundingBox (V3 (-a - b) (-b) (-a - b))
                (V3 (a + b) b (a + b))
    where
        a = radOuter
        b = radInner

shadowHitTorus :: Double -> Double -> BBox -> Ray -> Maybe Double
shadowHitTorus radOuter radInner bbox ray = boundingBoxHit bbox ray >> theHit
    where
        theHit = listToMaybe $ sort $ filter (> 0.1) roots
        a = radOuter
        b = radInner

        x1 = ray^.origin._x
        y1 = ray^.origin._y
        z1 = ray^.origin._z
        d1 = (signorm $ ray^.direction)^._x
        d2 = (signorm $ ray^.direction)^._y
        d3 = (signorm $ ray^.direction)^._z

        sum_d_sqrd = d1 * d1 + d2 * d2 + d3 * d3
        e = x1 * x1 + y1 * y1 + z1 * z1 - a * a - b * b
        f = x1 * d1 + y1 * d2 + z1 * d3
        four_a_sqrd = 4 * a * a

        c4 = e * e - four_a_sqrd * (b * b - y1 * y1)
        c3 = 4 * f * e + 2 * four_a_sqrd * y1 * d2
        c2 = 2 * sum_d_sqrd * e + 4 * f * f + four_a_sqrd * d2 * d2
        c1 = 4 * sum_d_sqrd * f
        c0 = 1

        roots = quartForm' (1e-30) c0 c1 c2 c3 c4

hitTorus :: Double -> Double -> BBox -> Material -> Ray -> Maybe (Shade, Double)
hitTorus radOuter radInner bbox mat ray =
    let makeShade tval = (sh, tval)
            where
              lhp = ray^.origin + (tval *^ ray^.direction)
              sh = defaultShade { _localHitPoint = lhp
                                , _material = mat
                                , _normal = computeNormal radOuter radInner lhp
                                }
    in makeShade <$> shadowHitTorus radOuter radInner bbox ray

computeNormal :: Double -> Double -> V3 Double -> V3 Double
computeNormal radOuter radInner hp =
    signorm $ V3 nx ny nz
    where
        a = radOuter
        b = radInner
        param_squared = a * a + b * b
        x = hp^._x
        y = hp^._y
        z = hp^._z
        sum_squared = x * x + y * y + z * z
        nx = 4.0 * x * (sum_squared - param_squared)
        ny = 4.0 * y * (sum_squared - param_squared + 2.0 * a * a)
        nz = 4.0 * z * (sum_squared - param_squared)
