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
             , _doPathShading = phongPathShading diffBrdf
             , _getLe = const cBlack
             }

phongFromColor :: Color -> Float -> Float -> Material
phongFromColor c ks e = phong
         (lambertian c 0.25)
         (lambertian c 0.65)
         (glossySpecular c ks e)

reflective :: Color -> Float -> Float -> Color -> Float -> Material
reflective c ks e cr kr =
    let ambBrdf = lambertian c 0.25
        diffBrdf = lambertian c 0.65
        glossyBrdf = glossySpecular c ks e
        reflBrdf = perfectSpecular cr kr
    in Material { _doShading = reflectiveShading ambBrdf diffBrdf glossyBrdf reflBrdf lightContrib
                , _doAreaShading = reflectiveShading ambBrdf diffBrdf glossyBrdf reflBrdf areaLightContrib
                , _doPathShading = reflectivePathShading diffBrdf reflBrdf
                , _getLe = const cBlack
                }

glossyReflective :: Color -> Float -> Float -> Color -> Float -> Float -> Material
glossyReflective c ks e cr kr er =
    let ambBrdf = lambertian c 0.25
        diffBrdf = lambertian c 0.25
        glossyBrdf = glossySpecular c ks e
        reflBrdf = glossySpecular cr kr er
    in Material { _doShading = glossyReflectiveShading ambBrdf diffBrdf glossyBrdf reflBrdf lightContrib
                , _doAreaShading = glossyReflectiveShading ambBrdf diffBrdf glossyBrdf reflBrdf areaLightContrib
                , _doPathShading = glossyReflectivePathShading glossyBrdf
                , _getLe = const cBlack
                }

glossyReflectivePathShading :: BRDF -> Shade -> Tracer -> TraceM Color
glossyReflectivePathShading glossyBrdf sh tracer = do
    let wo = (-1) *^ (sh^.shadeRay.direction)

    (pdf, fr, wi) <- (glossyBrdf^.brdfSampleF) sh wo

    let reflected_ray = Ray { _origin = sh^.localHitPoint
                            , _direction = wi
                            }
    traced <- (tracer^.doTrace) reflected_ray (sh^.depth + 1)
    return $ (fr * traced * (grey $ float2Double $ (sh^.normal) `dot` wi)) / (grey $ float2Double pdf)

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

reflectivePathShading :: BRDF -> BRDF -> Shade -> Tracer -> TraceM Color
reflectivePathShading diffBrdf reflBrdf sh tracer = do
    let wo = (-1) *^ (sh^.shadeRay.direction)

    (pdf, fr, wi) <- (reflBrdf^.brdfSampleF) sh wo
    let base = (diffBrdf^.brdfFunction) sh wo wi

    let reflected_ray = Ray { _origin = sh^.localHitPoint
                            , _direction = wi
                            }
    traced <- (tracer^.doTrace) reflected_ray (sh^.depth + 1)
    return $ base + (fr * traced * (grey $ float2Double $ (sh^.normal) `dot` wi)) / (grey $ float2Double pdf)

nullLD :: LightDir
nullLD = LD { _lightDir = V3 0 0 0
            , _lightSamplePoint = V3 0 0 0
            , _lightNormal = V3 0 0 0
            }

phongPathShading :: BRDF -> Shade -> Tracer -> TraceM Color
phongPathShading diffBrdf sh tracer = do
    let wo = -1 *^ sh^.shadeRay.direction
    (pdf, fr, wi) <- (diffBrdf^.brdfSampleF) sh wo

    let reflected_ray = Ray { _origin = sh^.localHitPoint
                            , _direction = wi
                            }
    traced <- (tracer^.doTrace) reflected_ray (sh^.depth + 1)
    return $ (fr * traced * (grey $ float2Double $ (sh^.normal) `dot` wi)) / (grey $ float2Double pdf)

phongShading :: BRDF -> BRDF -> BRDF
             -> (BRDF -> BRDF -> Light -> LightDir -> V3 Float -> Shade -> TraceM Color)
             -> Shade -> Tracer -> TraceM Color
phongShading ambBrdf diffBrdf glossyBrdf perLight sh _ = do
    w <- view tdWorld
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
        pdfValue = (light^.lightPDF) ld sh

    lColor <- (light^.lightColor) ld sh

    return $ ((diffBrdf^.brdfFunction) sh wo wi +
              (glossyBrdf^.brdfFunction) sh wo wi) *
             lColor *
             (grey $ float2Double gValue) *
             (grey $ float2Double ndotwi) /
             (grey $ float2Double pdfValue)
