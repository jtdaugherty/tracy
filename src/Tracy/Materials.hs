module Tracy.Materials where

import Control.Applicative
import Control.Lens
import Linear
import Data.Colour
import GHC.Float

import Tracy.Types

matte :: BRDF -> BRDF -> Material
matte ambBrdf diffBrdf =
    Material (ambShading ambBrdf diffBrdf)

phong :: BRDF -> BRDF -> BRDF -> Material
phong ambBrdf diffBrdf glossyBrdf =
    Material (phongShading ambBrdf diffBrdf glossyBrdf)

ambShading :: BRDF -> BRDF -> V3 Float -> Bool -> World -> Shade -> Color
ambShading ambBrdf diffBrdf sample enableShadows w sh =
    let wo = -1 *^ sh^.shadeRay.direction
        baseL = (ambBrdf^.brdfRho) (ambBrdf^.brdfData) sh wo * (w^.ambient.lightColor) w sample sh
        otherLs = getL <$> w^.lights
        getL light = let wi = (light^.lightDirection) sample sh
                         ndotwi = (sh^.normal) `dot` wi
                         shad = enableShadows && light^.lightShadows
                         in_shadow = (light^.inLightShadow) w shadowRay
                         shadowRay = Ray { _origin = sh^.localHitPoint
                                         , _direction = wi
                                         }
                     in if ndotwi > 0 && (not shad || (shad && not in_shadow))
                        then (diffBrdf^.brdfFunction) (diffBrdf^.brdfData) sh wo wi * (light^.lightColor) w sample sh * (grey $ float2Double ndotwi)
                        else 0.0
    in baseL + sum otherLs

phongShading :: BRDF -> BRDF -> BRDF -> V3 Float -> Bool -> World -> Shade -> Color
phongShading ambBrdf diffBrdf glossyBrdf sample enableShadows w sh =
    let wo = -1 *^ sh^.shadeRay.direction
        baseL = (ambBrdf^.brdfRho) (ambBrdf^.brdfData) sh wo * (w^.ambient.lightColor) w sample sh
        otherLs = getL <$> w^.lights
        getL light = let wi = (light^.lightDirection) sample sh
                         ndotwi = (sh^.normal) `dot` wi
                         shad = enableShadows && light^.lightShadows
                         in_shadow = (light^.inLightShadow) w shadowRay
                         shadowRay = Ray { _origin = sh^.localHitPoint
                                         , _direction = wi
                                         }
                     in if ndotwi > 0 && (not shad || (shad && not in_shadow))
                        then ((diffBrdf^.brdfFunction) (diffBrdf^.brdfData) sh wo wi +
                              (glossyBrdf^.brdfFunction) (glossyBrdf^.brdfData) sh wo wi) *
                                 (light^.lightColor) w sample sh * (grey $ float2Double ndotwi)
                        else 0.0
    in baseL + sum otherLs
