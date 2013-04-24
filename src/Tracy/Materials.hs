module Tracy.Materials where

import Control.Applicative
import Control.Lens
import Linear
import Data.Colour

import Tracy.Types

matte :: BRDF -> BRDF -> Material
matte ambBrdf diffBrdf =
    Material (ambShading ambBrdf diffBrdf)

ambShading :: BRDF -> BRDF -> World -> Shade -> Color
ambShading ambBrdf diffBrdf w sh =
    let wo = -1 *^ sh^.shadeRay.direction
        baseL = (ambBrdf^.brdfRho) (ambBrdf^.brdfData) sh wo * (w^.ambient.lightColor) sh
        otherLs = getL <$> w^.lights
        getL light = let wi = (light^.lightDirection) sh
                         ndotwi = (sh^.normal) `dot` wi
                     in if ndotwi > 0
                        then (diffBrdf^.brdfFunction) (diffBrdf^.brdfData) sh wo wi * (light^.lightColor) sh * (grey ndotwi)
                        else 0.0
    in baseL + sum otherLs
