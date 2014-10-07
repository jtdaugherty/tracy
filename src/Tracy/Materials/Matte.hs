module Tracy.Materials.Matte
  ( matteFromColor
  , matte
  )
  where

import Control.Applicative
import Control.Lens
import Linear
import Data.Colour
import GHC.Float

import Tracy.Types
import Tracy.BRDF
import Tracy.Samplers

matteFromColor :: Color -> Material
matteFromColor c = matte
        (lambertian (toUnitHemisphere jittered) c 0.25)
        (lambertian (toUnitHemisphere jittered) c 0.65)

matte :: BRDF -> BRDF -> Material
matte ambBrdf diffBrdf =
    Material (matteShading ambBrdf diffBrdf)

matteShading :: BRDF -> BRDF -> Shade -> TraceM Color
matteShading ambBrdf diffBrdf sh = do
    sample <- view tdHemiSample
    w <- view tdWorld

    let wo = -1 *^ sh^.shadeRay.direction
        baseL = (ambBrdf^.brdfRho) (ambBrdf^.brdfData) sh wo *
                (w^.ambient.lightColor) w sample sh
        otherLs = getL <$> w^.lights
        getL light = let wi = (light^.lightDirection) sample sh
                         ndotwi = (sh^.normal) `dot` wi
                         shad = w^.worldShadows && light^.lightShadows
                         in_shadow = (light^.inLightShadow) w shadowRay
                         shadowRay = Ray { _origin = sh^.localHitPoint
                                         , _direction = wi
                                         }
                     in if ndotwi > 0 && (not shad || (shad && not in_shadow))
                        then (diffBrdf^.brdfFunction) (diffBrdf^.brdfData) sh wo wi *
                             (light^.lightColor) w sample sh *
                             (grey $ float2Double ndotwi)
                        else 0.0

    return $ baseL + sum otherLs
