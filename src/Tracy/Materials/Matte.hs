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
    Material (matteShading ambBrdf diffBrdf)

matteShading :: BRDF -> BRDF -> Shade -> TraceM Color
matteShading ambBrdf diffBrdf sh = do
    w <- view tdWorld

    ambientColor <- (w^.ambient.lightColor) sh

    let wo = -1 *^ sh^.shadeRay.direction
        baseL = (ambBrdf^.brdfRho) (ambBrdf^.brdfData) sh wo * ambientColor
        getL light = do
            wi <- (light^.lightDirection) sh

            let ndotwi = (sh^.normal) `dot` wi
                shad = w^.worldShadows && light^.lightShadows
                shadowRay = Ray { _origin = sh^.localHitPoint
                                , _direction = wi
                                }

            in_shadow <- (light^.inLightShadow) shadowRay

            case ndotwi > 0 && (not shad || (shad && not in_shadow)) of
                True -> do
                        lColor <- (light^.lightColor) sh
                        return $ (diffBrdf^.brdfFunction) (diffBrdf^.brdfData) sh wo wi *
                                 lColor * (grey $ float2Double ndotwi)
                False -> return 0.0

    otherLs <- mapM getL $ w^.lights
    return $ baseL + sum otherLs
