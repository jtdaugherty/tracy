module Tracy.Lights.Ambient
  ( ambientLight
  )
  where

import Data.Colour
import GHC.Float
import Linear

import Tracy.Types

ambientLight :: Float -> Color -> Light
ambientLight ls c =
    Light False ambDir (ambColor ls c) ambShadow ambG ambPDF

ambG :: LightDir -> Shade -> Float
ambG = const $ const 1.0

ambPDF :: LightDir -> Shade -> Float
ambPDF = const $ const 1.0

ambDir :: Shade -> TraceM LightDir
ambDir = const $ return $ LD { _lightDir = V3 0 0 0
                             , _lightSamplePoint = V3 0 0 0
                             , _lightNormal = V3 0 0 0
                             }

ambColor :: Float -> Color -> LightDir -> Shade -> TraceM Color
ambColor ls c = const $ const $ return $ grey (float2Double ls) * c

ambShadow :: LightDir -> Ray -> TraceM Bool
ambShadow = const $ const $ return False
