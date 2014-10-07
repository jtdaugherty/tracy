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
    Light False ambDir (ambColor ls c) ambShadow

ambDir :: Shade -> TraceM (V3 Float)
ambDir = const $ return $ V3 0 0 0

ambColor :: Float -> Color -> Shade -> TraceM Color
ambColor ls c = const $ return $ grey (float2Double ls) * c

ambShadow :: Ray -> TraceM Bool
ambShadow = const $ return False
