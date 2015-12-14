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

import Tracy.Types

import Tracy.BRDF.GlossySpecular
import Tracy.BRDF.PerfectSpecular
import Tracy.BRDF.Lambertian

phong :: BRDF -> BRDF -> BRDF -> Material
phong ambBrdf diffBrdf glossyBrdf =
    Material { _doShading = phongShading ambBrdf diffBrdf glossyBrdf lightContrib
             , _doAreaShading = phongShading ambBrdf diffBrdf glossyBrdf areaLightContrib
             , _doPathShading = phongPathShading diffBrdf
             , _getLe = const cBlack
             }

phongFromColor :: Texture -> Double -> Double -> Material
phongFromColor t ks e = phong
         (lambertian t 0.25)
         (lambertian t 0.65)
         (glossySpecular t ks e)

reflective :: Texture -> Double -> Double -> Texture -> Double -> Material
reflective td ks e tr kr =
    let ambBrdf = lambertian td 0.25
        diffBrdf = lambertian td 0.65
        glossyBrdf = glossySpecular td ks e
        reflBrdf = perfectSpecular tr kr
    in Material { _doShading = reflectiveShading ambBrdf diffBrdf glossyBrdf reflBrdf lightContrib
                , _doAreaShading = reflectiveShading ambBrdf diffBrdf glossyBrdf reflBrdf areaLightContrib
                , _doPathShading = reflectivePathShading reflBrdf
                , _getLe = const cBlack
                }

glossyReflective :: Texture -> Double -> Double -> Texture -> Double -> Double -> Material
glossyReflective td ks e tr kr er =
    let ambBrdf = lambertian td 0.25
        diffBrdf = lambertian td 0.25
        glossyBrdf = glossySpecular td ks e
        reflBrdf = glossySpecular tr kr er
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
    return $ (fr * traced * (grey $ (sh^.normal) `dot` wi)) / (grey pdf)

glossyReflectiveShading :: BRDF -> BRDF -> BRDF -> BRDF
                        -> (BRDF -> BRDF -> Light -> LightDir -> V3 Double -> Shade -> TraceM Color)
                        -> Shade -> Tracer -> TraceM Color
glossyReflectiveShading ambBrdf diffBrdf glossyBrdf reflBrdf perLight sh tracer = do
    base <- phongShading ambBrdf diffBrdf glossyBrdf perLight sh tracer

    let wo = (-1) *^ (sh^.shadeRay.direction)

    (pdf, fr, wi) <- (reflBrdf^.brdfSampleF) sh wo

    let reflected_ray = Ray { _origin = sh^.localHitPoint
                            , _direction = wi
                            }
    traced <- (tracer^.doTrace) reflected_ray (sh^.depth + 1)
    return $ base + (fr * traced * (grey $ (sh^.normal) `dot` wi)) / (grey pdf)

reflectiveShading :: BRDF -> BRDF -> BRDF -> BRDF
                  -> (BRDF -> BRDF -> Light -> LightDir -> V3 Double -> Shade -> TraceM Color)
                  -> Shade -> Tracer -> TraceM Color
reflectiveShading ambBrdf diffBrdf glossyBrdf reflBrdf perLight sh tracer = do
    base <- phongShading ambBrdf diffBrdf glossyBrdf perLight sh tracer

    let wo = (-1) *^ (sh^.shadeRay.direction)

    (_, fr, wi) <- (reflBrdf^.brdfSampleF) sh wo

    let reflected_ray = Ray { _origin = sh^.localHitPoint
                            , _direction = wi
                            }
    traced <- (tracer^.doTrace) reflected_ray (sh^.depth + 1)
    return $ base + (fr * traced * (grey $ (sh^.normal) `dot` wi))

reflectivePathShading :: BRDF -> Shade -> Tracer -> TraceM Color
reflectivePathShading reflBrdf sh tracer = do
    let wo = (-1) *^ (sh^.shadeRay.direction)

    (pdf, fr, wi) <- (reflBrdf^.brdfSampleF) sh wo

    let reflected_ray = Ray { _origin = sh^.localHitPoint
                            , _direction = wi
                            }
    traced <- (tracer^.doTrace) reflected_ray (sh^.depth + 1)
    return $ (fr * traced * (grey $ (sh^.normal) `dot` wi)) / (grey pdf)

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
    traced <- (tracer^.doTrace) reflected_ray (sh^.depth + Depth 1)
    return $ (fr * traced * (grey $ (sh^.normal) `dot` wi)) / (grey pdf)

phongShading :: BRDF -> BRDF -> BRDF
             -> (BRDF -> BRDF -> Light -> LightDir -> V3 Double -> Shade -> TraceM Color)
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

lightContrib :: BRDF -> BRDF -> Light -> LightDir -> V3 Double -> Shade -> TraceM Color
lightContrib diffBrdf glossyBrdf light ld wo sh = do
    let wi = ld^.lightDir
        ndotwi = (sh^.normal) `dot` wi

    lColor <- (light^.lightColor) ld sh
    return $ ((diffBrdf^.brdfFunction) sh wo wi +
             (glossyBrdf^.brdfFunction) sh wo wi) *
             lColor * (grey ndotwi)

areaLightContrib :: BRDF -> BRDF -> Light -> LightDir -> V3 Double -> Shade -> TraceM Color
areaLightContrib diffBrdf glossyBrdf light ld wo sh = do
    let wi = ld^.lightDir
        ndotwi = (sh^.normal) `dot` wi
        gValue = (light^.lightG) ld sh
        pdfValue = (light^.lightPDF) ld sh

    lColor <- (light^.lightColor) ld sh

    return $ ((diffBrdf^.brdfFunction) sh wo wi +
              (glossyBrdf^.brdfFunction) sh wo wi) *
             lColor *
             (grey gValue) *
             (grey ndotwi) /
             (grey pdfValue)
