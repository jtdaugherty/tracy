module Tracy.Lights.Point
  ( pointLight
  )
  where

import Control.Applicative
import Control.Lens
import Data.Colour
import Data.Maybe
import GHC.Float
import Linear

import Tracy.Types

pointLight :: Bool -> Float -> Color -> V3 Float -> Light
pointLight sh ls c loc =
    Light sh (ptDir ls c loc) (ptColor ls c) (ptShadow loc) ptG ptPDF

ptG :: LightDir -> Shade -> Float
ptG = const $ const 1.0

ptPDF :: Shade -> Float
ptPDF = const 1.0

ptShadow :: V3 Float -> LightDir -> Ray -> TraceM Bool
ptShadow loc _ld r = do
    hitFuncs <- view tdWorldShadowHitFuncs
    let results = hitFuncs <*> pure r
        d = distance loc (r^.origin)
    return $ (not . null) $ filter (< d) $ catMaybes results

ptDir :: Float -> Color -> V3 Float -> Shade -> TraceM LightDir
ptDir _ _ loc sh = do
    let d = signorm $ loc - (sh^.localHitPoint)
    return $ LD { _lightDir = d
                , _lightNormal = V3 0 0 0
                , _lightSamplePoint = V3 0 0 0
                }

ptColor :: Float -> Color -> LightDir -> Shade -> TraceM Color
ptColor ls c = const $ const $ return $ grey (float2Double ls) * c
