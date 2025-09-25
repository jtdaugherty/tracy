module Tracy.Materials.Mix
  ( mix
  )
  where

import Control.Lens
import Data.Colour

import Tracy.Types

-- |The specified amount (0 .. 1) specifies how much the first material
-- contributes; the rest is the second.
mix :: Double -> Material -> Material -> Material
mix amt m1 m2 =
    Material { _doShading = mixShading amt m1 m2
             , _doAreaShading = mixAreaShading amt m1 m2
             , _doPathShading = mixPathShading amt m1 m2
             , _getLe = mixGetLe amt m1 m2
             }

theMix :: Double -> Color -> Color -> Color
theMix amt v1 v2 = (v1 * grey amt) + (v2 * (grey (1-amt)))

mixShading :: Double -> Material -> Material -> Shade -> Tracer -> TraceM Color
mixShading amt m1 m2 sh t
  | amt == 1.0 = v1
  | amt == 0.0 = v2
  | otherwise = theMix amt <$> v1 <*> v2
  where
    v1 = (m1^.doShading) sh t
    v2 = (m2^.doShading) sh t

mixAreaShading :: Double -> Material -> Material -> Shade -> Tracer -> TraceM Color
mixAreaShading amt m1 m2 sh t
  | amt == 1.0 = v1
  | amt == 0.0 = v2
  | otherwise = theMix amt <$> v1 <*> v2
  where
    v1 = (m1^.doAreaShading) sh t
    v2 = (m2^.doAreaShading) sh t

mixPathShading :: Double -> Material -> Material -> Shade -> Tracer -> TraceM Color
mixPathShading amt m1 m2 sh t
  | amt == 1.0 = v1
  | amt == 0.0 = v2
  | otherwise = theMix amt <$> v1 <*> v2
  where
    v1 = (m1^.doPathShading) sh t
    v2 = (m2^.doPathShading) sh t

mixGetLe :: Double -> Material -> Material -> Shade -> Color
mixGetLe amt m1 m2 sh
  | amt == 1.0 = v1
  | amt == 0.0 = v2
  | otherwise = theMix amt v1 v2
    where
      v1 = (m1^.getLe) sh
      v2 = (m2^.getLe) sh
