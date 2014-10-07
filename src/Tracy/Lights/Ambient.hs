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

ambDir :: V3 Float -> Shade -> V3 Float
ambDir _ = const $ V3 0 0 0

ambColor :: Float -> Color -> World -> V3 Float -> Shade -> Color
ambColor ls c _ _ = const $ grey (float2Double ls) * c

ambShadow :: World -> Ray -> Bool
ambShadow _ _ = False
