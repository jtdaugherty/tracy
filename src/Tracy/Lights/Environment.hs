module Tracy.Lights.Environment
  ( environmentLight
  )
  where

import Control.Lens
import Data.Maybe
import Linear

import Tracy.Types

environmentLight :: Bool -> Material -> Light
environmentLight sh m =
    Light sh envDir (envColor m) envShadow envG envPDF

envG :: LightDir -> Shade -> Double
envG = const $ const 1.0

envPDF :: LightDir -> Shade -> Double
envPDF ld sh =
    let ndotw = (sh^.normal) `dot` (ld^.lightDir)
    in ndotw / pi

envUVW :: Shade -> (V3 Double, V3 Double, V3 Double)
envUVW sh =
    let w = sh^.normal
        v = signorm $ (V3 0.0034 1 0.0071) `cross` w
        u = v `cross` w
    in (u, v, w)

envDir :: Shade -> TraceM LightDir
envDir sh = do
    sample <- view tdHemiSample
    let (u, v, w) = envUVW sh
        ldir = sample^._x *^ u + sample^._y *^ v + sample^._z *^ w
    return $ LD { _lightDir = ldir
                , _lightSamplePoint = V3 0 0 0
                , _lightNormal = V3 0 0 0
                }

envShadow :: LightDir -> Ray -> TraceM Bool
envShadow _ld r = do
    w <- view tdWorld
    let results = (w^..objects.folded.shadow_hit) <*> pure r
    return $ (not . null) $ catMaybes results

envColor :: Material -> LightDir -> Shade -> TraceM Color
envColor m _ sh = return $ (m^.getLe) sh
