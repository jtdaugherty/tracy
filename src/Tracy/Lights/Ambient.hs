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

ambG :: Shade -> Float
ambG = const 1.0

ambPDF :: Shade -> Float
ambPDF = const 1.0

ambDir :: Shade -> TraceM LightDir
ambDir = const $ return $ LD { _lightDir = V3 0 0 0
                             }

ambColor :: Float -> Color -> Shade -> TraceM Color
ambColor ls c = const $ return $ grey (float2Double ls) * c

ambShadow :: Ray -> TraceM Bool
ambShadow = const $ return False
