module Tracy.Materials.Phong
  ( phongFromColor
  , phong
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

phong :: BRDF -> BRDF -> BRDF -> Material
phong ambBrdf diffBrdf glossyBrdf =
    Material (phongShading ambBrdf diffBrdf glossyBrdf)

phongFromColor :: Color -> Float -> Material
phongFromColor c e = phong
         (lambertian (toUnitHemisphere jittered) c 0.25)
         (lambertian (toUnitHemisphere jittered) c 0.65)
         (glossySpecular c e)

phongShading :: BRDF -> BRDF -> BRDF -> Shade -> TraceM Color
phongShading ambBrdf diffBrdf glossyBrdf sh = do
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
                        then ((diffBrdf^.brdfFunction) (diffBrdf^.brdfData) sh wo wi +
                              (glossyBrdf^.brdfFunction) (glossyBrdf^.brdfData) sh wo wi) *
                                 (light^.lightColor) w sample sh * (grey $ float2Double ndotwi)
                        else 0.0

    return $ baseL + sum otherLs
