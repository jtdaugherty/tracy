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

mixShading :: Double -> Material -> Material -> Shade -> Tracer -> TraceM Color
mixShading amt m1 m2 sh t = do
    v1 <- (m1^.doShading) sh t
    v2 <- (m2^.doShading) sh t
    return $ (v1 * grey amt) + (v2 * (grey $ 1-amt))

mixAreaShading :: Double -> Material -> Material -> Shade -> Tracer -> TraceM Color
mixAreaShading amt m1 m2 sh t = do
    v1 <- (m1^.doAreaShading) sh t
    v2 <- (m2^.doAreaShading) sh t
    return $ (v1 * grey amt) + (v2 * (grey $ 1-amt))

mixPathShading :: Double -> Material -> Material -> Shade -> Tracer -> TraceM Color
mixPathShading amt m1 m2 sh t = do
    v1 <- (m1^.doPathShading) sh t
    v2 <- (m2^.doPathShading) sh t
    return $ (v1 * grey amt) + (v2 * (grey $ 1-amt))

mixGetLe :: Double -> Material -> Material -> Shade -> Color
mixGetLe amt m1 m2 sh = (v1 * grey amt) + (v2 * (grey $ 1-amt))
    where
      v1 = (m1^.getLe) sh
      v2 = (m2^.getLe) sh
