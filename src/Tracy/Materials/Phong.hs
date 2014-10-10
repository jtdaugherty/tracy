module Tracy.Materials.Phong
  ( phongFromColor
  , phong
  , reflective
  , glossyReflective
  )
  where

import Control.Lens
import Linear
import Data.Colour
import GHC.Float

import Tracy.Types
import Tracy.BRDF

phong :: BRDF -> BRDF -> BRDF -> Material
phong ambBrdf diffBrdf glossyBrdf =
    Material { _doShading = phongShading ambBrdf diffBrdf glossyBrdf lightContrib
             , _doAreaShading = phongShading ambBrdf diffBrdf glossyBrdf areaLightContrib
             , _getLe = const cBlack
             }

phongFromColor :: Color -> Float -> Material
phongFromColor c e = phong
         (lambertian c 0.25)
         (lambertian c 0.65)
         (glossySpecular c e)

reflective :: Color -> Float -> Color -> Float -> Material
reflective c e cr kr =
    let ambBrdf = lambertian c 0.25
        diffBrdf = lambertian c 0.65
        glossyBrdf = glossySpecular c e
        reflBrdf = perfectSpecular cr kr
    in Material { _doShading = reflectiveShading ambBrdf diffBrdf glossyBrdf reflBrdf lightContrib
                , _doAreaShading = reflectiveShading ambBrdf diffBrdf glossyBrdf reflBrdf areaLightContrib
                , _getLe = const cBlack
                }

glossyReflective :: Color -> Float -> Color -> Float -> Material
glossyReflective c e cr kr =
    let ambBrdf = lambertian c 0.25
        diffBrdf = lambertian c 0.25
        glossyBrdf = glossySpecular c e
        reflBrdf = glossySpecular cr kr
    in Material { _doShading = glossyReflectiveShading ambBrdf diffBrdf glossyBrdf reflBrdf lightContrib
                , _doAreaShading = glossyReflectiveShading ambBrdf diffBrdf glossyBrdf reflBrdf areaLightContrib
                , _getLe = const cBlack
                }

glossyReflectiveShading :: BRDF -> BRDF -> BRDF -> BRDF
                        -> (BRDF -> BRDF -> Light -> LightDir -> V3 Float -> Shade -> TraceM Color)
                        -> Shade -> Tracer -> TraceM Color
glossyReflectiveShading ambBrdf diffBrdf glossyBrdf reflBrdf perLight sh tracer = do
    base <- phongShading ambBrdf diffBrdf glossyBrdf perLight sh tracer

    let wo = (-1) *^ (sh^.shadeRay.direction)

    (pdf, fr, wi) <- (reflBrdf^.brdfSampleF) sh wo

    let reflected_ray = Ray { _origin = sh^.localHitPoint
                            , _direction = wi
                            }
    traced <- (tracer^.doTrace) reflected_ray (sh^.depth + 1)
    return $ base + (fr * traced * (grey $ float2Double $ (sh^.normal) `dot` wi)) / (grey $ float2Double pdf)

reflectiveShading :: BRDF -> BRDF -> BRDF -> BRDF
                  -> (BRDF -> BRDF -> Light -> LightDir -> V3 Float -> Shade -> TraceM Color)
                  -> Shade -> Tracer -> TraceM Color
reflectiveShading ambBrdf diffBrdf glossyBrdf reflBrdf perLight sh tracer = do
    base <- phongShading ambBrdf diffBrdf glossyBrdf perLight sh tracer

    let wo = (-1) *^ (sh^.shadeRay.direction)

    (_, fr, wi) <- (reflBrdf^.brdfSampleF) sh wo

    let reflected_ray = Ray { _origin = sh^.localHitPoint
                            , _direction = wi
                            }
    traced <- (tracer^.doTrace) reflected_ray (sh^.depth + 1)
    return $ base + (fr * traced * (grey $ float2Double $ (sh^.normal) `dot` wi))

phongShading :: BRDF -> BRDF -> BRDF
             -> (BRDF -> BRDF -> Light -> LightDir -> V3 Float -> Shade -> TraceM Color)
             -> Shade -> Tracer -> TraceM Color
phongShading ambBrdf diffBrdf glossyBrdf perLight sh _ = do
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
                True -> perLight diffBrdf glossyBrdf light ld wo sh
                False -> return 0.0

    otherLs <- mapM getL $ w^.lights
    return $ baseL + sum otherLs

lightContrib :: BRDF -> BRDF -> Light -> LightDir -> V3 Float -> Shade -> TraceM Color
lightContrib diffBrdf glossyBrdf light ld wo sh = do
    let wi = ld^.lightDir
        ndotwi = (sh^.normal) `dot` wi

    lColor <- (light^.lightColor) ld sh
    return $ ((diffBrdf^.brdfFunction) sh wo wi +
             (glossyBrdf^.brdfFunction) sh wo wi) *
             lColor * (grey $ float2Double ndotwi)

areaLightContrib :: BRDF -> BRDF -> Light -> LightDir -> V3 Float -> Shade -> TraceM Color
areaLightContrib diffBrdf glossyBrdf light ld wo sh = do
    let wi = ld^.lightDir
        ndotwi = (sh^.normal) `dot` wi
        gValue = (light^.lightG) ld sh
        pdfValue = (light^.lightPDF) sh

    lColor <- (light^.lightColor) ld sh

    return $ ((diffBrdf^.brdfFunction) sh wo wi +
              (glossyBrdf^.brdfFunction) sh wo wi) *
             lColor *
             (grey $ float2Double gValue) *
             (grey $ float2Double ndotwi) /
             (grey $ float2Double pdfValue)
