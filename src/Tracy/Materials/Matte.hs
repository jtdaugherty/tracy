module Tracy.Materials.Matte
  ( matteFromColor
  , matte
  )
  where

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
    Material { _doShading = matteShading ambBrdf diffBrdf lightContrib
             , _doAreaShading = matteShading ambBrdf diffBrdf areaLightContrib
             , _getLe = const cBlack
             }

matteShading :: BRDF -> BRDF
             -> (BRDF -> Light -> LightDir -> V3 Float -> Shade -> TraceM Color)
             -> Shade -> Tracer -> TraceM Color
matteShading ambBrdf diffBrdf perLight sh _ = do
    w <- view tdWorld

    let nullLD = LD { _lightDir = V3 0 0 0
                    , _lightSamplePoint = V3 0 0 0
                    , _lightNormal = V3 0 0 0
                    }
    ambientColor <- (w^.ambient.lightColor) nullLD sh

    let wo = -1 *^ sh^.shadeRay.direction
        baseL = (ambBrdf^.brdfRho) sh wo * ambientColor
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
                True -> perLight diffBrdf light ld wo sh
                False -> return 0.0

    otherLs <- mapM getL $ w^.lights
    return $ baseL + sum otherLs

lightContrib :: BRDF -> Light -> LightDir -> V3 Float -> Shade -> TraceM Color
lightContrib diffBrdf light ld wo sh = do
    let wi = ld^.lightDir
        ndotwi = (sh^.normal) `dot` wi

    lColor <- (light^.lightColor) ld sh
    return $ (diffBrdf^.brdfFunction) sh wo wi *
             lColor * (grey $ float2Double ndotwi)

areaLightContrib :: BRDF -> Light -> LightDir -> V3 Float -> Shade -> TraceM Color
areaLightContrib diffBrdf light ld wo sh = do
    let wi = ld^.lightDir
        ndotwi = (sh^.normal) `dot` wi
        gValue = (light^.lightG) ld sh
        pdfValue = (light^.lightPDF) sh

    lColor <- (light^.lightColor) ld sh

    let v =  (diffBrdf^.brdfFunction) sh wo wi *
             lColor *
             (grey $ float2Double gValue) *
             (grey $ float2Double ndotwi) /
             (grey $ float2Double pdfValue)
    return v
