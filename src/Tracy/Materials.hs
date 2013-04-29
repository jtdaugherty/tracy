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

ambShading :: BRDF -> BRDF -> World -> Shade -> Color
ambShading ambBrdf diffBrdf w sh =
    let wo = -1 *^ sh^.shadeRay.direction
        baseL = (ambBrdf^.brdfRho) (ambBrdf^.brdfData) sh wo * (w^.ambient.lightColor) sh
        otherLs = getL <$> w^.lights
        getL light = let wi = (light^.lightDirection) sh
                         ndotwi = (sh^.normal) `dot` wi
                     in if ndotwi > 0
                        then (diffBrdf^.brdfFunction) (diffBrdf^.brdfData) sh wo wi * (light^.lightColor) sh * (grey $ float2Double ndotwi)
                        else 0.0
    in baseL + sum otherLs

phongShading :: BRDF -> BRDF -> BRDF -> World -> Shade -> Color
phongShading ambBrdf diffBrdf glossyBrdf w sh =
    let wo = -1 *^ sh^.shadeRay.direction
        baseL = (ambBrdf^.brdfRho) (ambBrdf^.brdfData) sh wo * (w^.ambient.lightColor) sh
        otherLs = getL <$> w^.lights
        getL light = let wi = (light^.lightDirection) sh
                         ndotwi = (sh^.normal) `dot` wi
                     in if ndotwi > 0
                        then ((diffBrdf^.brdfFunction) (diffBrdf^.brdfData) sh wo wi +
                              (glossyBrdf^.brdfFunction) (glossyBrdf^.brdfData) sh wo wi) *
                                 (light^.lightColor) sh * (grey $ float2Double ndotwi)
                        else 0.0
    in baseL + sum otherLs
