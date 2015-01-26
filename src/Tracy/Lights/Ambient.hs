module Tracy.Lights.Ambient
  ( ambientLight
  )
  where

import Data.Colour
import Linear

import Tracy.Types

ambientLight :: Double -> Color -> Light
ambientLight ls c =
    Light False ambDir (ambColor ls c) ambShadow ambG ambPDF

ambG :: LightDir -> Shade -> Double
ambG = const $ const 1.0

ambPDF :: LightDir -> Shade -> Double
ambPDF = const $ const 1.0

ambDir :: Shade -> TraceM LightDir
ambDir = const $ return $ LD { _lightDir = V3 0 0 0
                             , _lightSamplePoint = V3 0 0 0
                             , _lightNormal = V3 0 0 0
                             }

ambColor :: Double -> Color -> LightDir -> Shade -> TraceM Color
ambColor ls c = const $ const $ return $ grey ls * c

ambShadow :: LightDir -> Ray -> TraceM Bool
ambShadow = const $ const $ return False
