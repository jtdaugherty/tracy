module Tracy.Materials.Matte
  ( matteFromTexture
  , matteFromColor
  , matte
  )
  where

import Control.Lens
import Linear
import Data.Colour

import Tracy.Types
import Tracy.BRDF.Lambertian
import Tracy.Textures.ConstantColor

matteFromColor :: Color -> Material
matteFromColor c =
    let t = constantColor c
    in matte (lambertian t 0.25) (lambertian t 0.65)

matteFromTexture :: Texture -> Material
matteFromTexture t = matte
        (lambertian t 0.25)
        (lambertian t 0.65)

matte :: BRDF -> BRDF -> Material
matte ambBrdf diffBrdf =
    Material { _doShading = matteShading ambBrdf diffBrdf lightContrib
             , _doAreaShading = matteShading ambBrdf diffBrdf areaLightContrib
             , _doPathShading = mattePathShading diffBrdf
             , _getLe = const cBlack
             }

mattePathShading :: BRDF -> Shade -> Tracer -> TraceM Color
mattePathShading diffBrdf sh tracer = do
    let wo = (-1) *^ (sh^.shadeRay.direction)
    (pdf, c, wi) <- (diffBrdf^.brdfSampleF) sh wo
    let ndotwi = (sh^.normal) `dot` wi
        refl_ray = Ray { _direction = wi
                       , _origin = sh^.localHitPoint
                       }

    next <- (tracer^.doTrace) refl_ray ((sh^.depth) + 1)
    return $ c * next * (grey $ ndotwi / pdf)

matteShading :: BRDF -> BRDF
             -> (BRDF -> Light -> LightDir -> V3 Double -> Shade -> TraceM Color)
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

lightContrib :: BRDF -> Light -> LightDir -> V3 Double -> Shade -> TraceM Color
lightContrib diffBrdf light ld wo sh = do
    let wi = ld^.lightDir
        ndotwi = (sh^.normal) `dot` wi

    lColor <- (light^.lightColor) ld sh
    return $ (diffBrdf^.brdfFunction) sh wo wi *
             lColor * (grey ndotwi)

areaLightContrib :: BRDF -> Light -> LightDir -> V3 Double -> Shade -> TraceM Color
areaLightContrib diffBrdf light ld wo sh = do
    let wi = ld^.lightDir
        ndotwi = (sh^.normal) `dot` wi
        gValue = (light^.lightG) ld sh
        pdfValue = (light^.lightPDF) ld sh

    lColor <- (light^.lightColor) ld sh

    let v =  (diffBrdf^.brdfFunction) sh wo wi *
             lColor *
             (grey gValue) *
             (grey ndotwi) /
             (grey pdfValue)
    return v
