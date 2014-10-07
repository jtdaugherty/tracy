module Tracy.Lights.AmbientOccluder
  ( ambientOccluder
  )
  where

import Control.Applicative
import Control.Lens
import Data.Colour
import Data.Maybe
import GHC.Float
import Linear

import Tracy.Types

ambientOccluder :: Color -> Color -> Float -> Light
ambientOccluder c min_amount ls =
    Light True ambOccDir (ambOccColor ls c min_amount) ambOccShadow

ambOccUVW :: Shade -> (V3 Float, V3 Float, V3 Float)
ambOccUVW sh =
    let w = sh^.normal
        v = signorm $ w `cross` (V3 0.0072 1.0 0.0034)
        u = v `cross` w
    in (u, v, w)

ambOccDir :: Shade -> TraceM (V3 Float)
ambOccDir sh = do
    sample <- view tdHemiSample
    let (u, v, w) = ambOccUVW sh
    return $ sample^._x *^ u + sample^._y *^ v + sample^._z *^ w

ambOccShadow :: Ray -> TraceM Bool
ambOccShadow r = do
    w <- view tdWorld
    let results = (w^..objects.folded.shadow_hit) <*> pure r
    return $ (not . null) $ catMaybes results

ambOccColor :: Float -> Color -> Color -> Shade -> TraceM Color
ambOccColor ls color min_amount sh = do
    shadow_d <- ambOccDir sh

    let shadow_o = sh^.localHitPoint
        shadow_r = Ray shadow_o shadow_d

    shad <- ambOccShadow shadow_r
    return $ if shad
             then min_amount * (grey $ float2Double ls) * color
             else (grey $ float2Double ls) * color
