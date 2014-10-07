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

ambOccDir :: V3 Float -> Shade -> V3 Float
ambOccDir sample sh =
    let (u, v, w) = ambOccUVW sh
    in sample^._x *^ u + sample^._y *^ v + sample^._z *^ w

ambOccShadow :: World -> Ray -> Bool
ambOccShadow w r =
    (not . null) $ catMaybes results
    where
        results = (w^..objects.folded.shadow_hit) <*> pure r

ambOccColor :: Float -> Color -> Color -> World -> V3 Float -> Shade -> Color
ambOccColor ls color min_amount w sample sh =
    let shadow_o = sh^.localHitPoint
        shadow_d = ambOccDir sample sh
        shadow_r = Ray shadow_o shadow_d
    in if ambOccShadow w shadow_r
       then min_amount * (grey $ float2Double ls) * color
       else (grey $ float2Double ls) * color
