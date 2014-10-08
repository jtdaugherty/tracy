module Tracy.Lights.Area
  ( areaLight
  )
  where

import Control.Applicative
import Control.Lens
import Data.Colour
import Data.Maybe
import Linear

import Tracy.Types
import Tracy.Util

areaLight :: Bool -> Object -> Light
areaLight sh o =
    let ali = case o^.areaLightImpl of
                Nothing -> error "Objects used with area lights must provide an area light implementation"
                Just v -> v

    in Light { _lightShadows = sh
             , _lightDirection = areaLightDir ali
             , _lightColor = areaLightColor (o^.objectMaterial)
             , _inLightShadow = if sh
                                then areaLightInShadow
                                else const $ const $ return False
             , _lightG = areaLightG
             , _lightPDF = ali^.objectPDF
             }

areaLightDir :: ObjectAreaLightImpl -> Shade -> TraceM LightDir
areaLightDir ali sh = do
    oSample <- ali^.objectSurfaceSample
    let wi = signorm $ oSample - sh^.localHitPoint
        oNormal = (ali^.objectGetNormal) oSample

    return $ LD { _lightDir = wi
                , _lightSamplePoint = oSample
                , _lightNormal = oNormal
                }

areaLightColor :: Material -> LightDir -> Shade -> TraceM Color
areaLightColor m ld sh = do
    let ndotd = ((-1) *^ (ld^.lightNormal)) `dot` (ld^.lightDir)
    if ndotd > 0 then
        return $ (m^.getLe) sh else
        return cBlack

areaLightInShadow :: LightDir -> Ray -> TraceM Bool
areaLightInShadow ld r = do
    hitFuncs <- view tdWorldShadowHitFuncs
    let ts = (ld^.lightSamplePoint - r^.origin) `dot` (r^.direction)
        results = hitFuncs <*> pure r
    return $ (not . null) $ filter (< ts) $ catMaybes results

areaLightG :: LightDir -> Shade -> Float
areaLightG ld sh =
    let ndotd = ((-1) *^ (ld^.lightNormal)) `dot` (ld^.lightDir)
        d2 = dSquared (ld^.lightSamplePoint) (sh^.localHitPoint)
    in ndotd / d2
