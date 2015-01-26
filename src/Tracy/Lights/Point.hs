module Tracy.Lights.Point
  ( pointLight
  )
  where

import Control.Applicative
import Control.Lens
import Data.Colour
import Data.Maybe
import Linear

import Tracy.Types

pointLight :: Bool -> Double -> Color -> V3 Double -> Light
pointLight sh ls c loc =
    Light sh (ptDir ls c loc) (ptColor ls c) (ptShadow loc) ptG ptPDF

ptG :: LightDir -> Shade -> Double
ptG = const $ const 1.0

ptPDF :: LightDir -> Shade -> Double
ptPDF = const $ const 1.0

ptShadow :: V3 Double -> LightDir -> Ray -> TraceM Bool
ptShadow loc _ld r = do
    hitFuncs <- view tdWorldShadowHitFuncs
    let results = hitFuncs <*> pure r
        d = distance loc (r^.origin)
    return $ (not . null) $ filter (< d) $ catMaybes results

ptDir :: Double -> Color -> V3 Double -> Shade -> TraceM LightDir
ptDir _ _ loc sh = do
    let d = signorm $ loc - (sh^.localHitPoint)
    return $ LD { _lightDir = d
                , _lightNormal = V3 0 0 0
                , _lightSamplePoint = V3 0 0 0
                }

ptColor :: Double -> Color -> LightDir -> Shade -> TraceM Color
ptColor ls c = const $ const $ return $ grey ls * c
