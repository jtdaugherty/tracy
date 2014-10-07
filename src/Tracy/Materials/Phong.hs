module Tracy.Materials.Phong
  ( phongFromColor
  , phong
  )
  where

import Control.Lens
import Linear
import Data.Colour
import GHC.Float

import Tracy.Types
import Tracy.BRDF
import Tracy.Samplers

phong :: BRDF -> BRDF -> BRDF -> Material
phong ambBrdf diffBrdf glossyBrdf =
    Material { _doShading = phongShading ambBrdf diffBrdf glossyBrdf
             , _doAreaShading = phongShading ambBrdf diffBrdf glossyBrdf
             }

phongFromColor :: Color -> Float -> Material
phongFromColor c e = phong
         (lambertian (toUnitHemisphere jittered) c 0.25)
         (lambertian (toUnitHemisphere jittered) c 0.65)
         (glossySpecular c e)

phongShading :: BRDF -> BRDF -> BRDF -> Shade -> TraceM Color
phongShading ambBrdf diffBrdf glossyBrdf sh = do
    w <- view tdWorld

    ambientColor <- (w^.ambient.lightColor) sh

    let wo = -1 *^ sh^.shadeRay.direction
        baseL = (ambBrdf^.brdfRho) (ambBrdf^.brdfData) sh wo * ambientColor
        getL light = do
            ld <- (light^.lightDirection) sh
            let wi = ld^.lightDir
                ndotwi = (sh^.normal) `dot` wi
                shad = w^.worldShadows && light^.lightShadows
                shadowRay = Ray { _origin = sh^.localHitPoint
                                , _direction = wi
                                }

            in_shadow <- (light^.inLightShadow) shadowRay

            case ndotwi > 0 && (not shad || (shad && not in_shadow)) of
                True -> do
                    lColor <- (light^.lightColor) sh
                    return $ ((diffBrdf^.brdfFunction) (diffBrdf^.brdfData) sh wo wi +
                             (glossyBrdf^.brdfFunction) (glossyBrdf^.brdfData) sh wo wi) *
                             lColor * (grey $ float2Double ndotwi)
                False -> return 0.0

    otherLs <- mapM getL $ w^.lights
    return $ baseL + sum otherLs
