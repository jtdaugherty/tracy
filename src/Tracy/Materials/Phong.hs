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
    Material { _doShading = phongShading ambBrdf diffBrdf glossyBrdf lightContrib
             , _doAreaShading = phongShading ambBrdf diffBrdf glossyBrdf areaLightContrib
             }

phongFromColor :: Color -> Float -> Material
phongFromColor c e = phong
         (lambertian (toUnitHemisphere jittered) c 0.25)
         (lambertian (toUnitHemisphere jittered) c 0.65)
         (glossySpecular c e)

phongShading :: BRDF -> BRDF -> BRDF
             -> (BRDF -> BRDF -> Light -> LightDir -> V3 Float -> Shade -> TraceM Color)
             -> Shade -> TraceM Color
phongShading ambBrdf diffBrdf glossyBrdf perLight sh = do
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

            in_shadow <- (light^.inLightShadow) ld shadowRay

            case ndotwi > 0 && (not shad || (shad && not in_shadow)) of
                True -> perLight diffBrdf glossyBrdf light ld wo sh
                False -> return 0.0

    otherLs <- mapM getL $ w^.lights
    return $ baseL + sum otherLs

lightContrib :: BRDF -> BRDF -> Light -> LightDir -> V3 Float -> Shade -> TraceM Color
lightContrib diffBrdf glossyBrdf light ld wo sh = do
    let wi = ld^.lightDir
        ndotwi = (sh^.normal) `dot` wi

    lColor <- (light^.lightColor) ld sh
    return $ ((diffBrdf^.brdfFunction) (diffBrdf^.brdfData) sh wo wi +
             (glossyBrdf^.brdfFunction) (glossyBrdf^.brdfData) sh wo wi) *
             lColor * (grey $ float2Double ndotwi)

areaLightContrib :: BRDF -> BRDF -> Light -> LightDir -> V3 Float -> Shade -> TraceM Color
areaLightContrib diffBrdf glossyBrdf light ld wo sh = do
    let wi = ld^.lightDir
        ndotwi = (sh^.normal) `dot` wi
        gValue = (light^.lightG) ld sh
        pdfValue = (light^.lightPDF) sh

    lColor <- (light^.lightColor) ld sh

    return $ ((diffBrdf^.brdfFunction) (diffBrdf^.brdfData) sh wo wi +
              (glossyBrdf^.brdfFunction) (glossyBrdf^.brdfData) sh wo wi) *
             lColor *
             (grey $ float2Double gValue) *
             (grey $ float2Double ndotwi) /
             (grey $ float2Double pdfValue)
