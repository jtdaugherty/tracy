module Tracy.Lights where

import Control.Lens
import Linear
import Data.Colour
import GHC.Float

import Tracy.Types

ambientLight :: Float -> Color -> Light
ambientLight ls c =
    Light False ambDir (ambColor ls c)

ambDir :: Shade -> V3 Float
ambDir = const $ V3 0 0 0

ambColor :: Float -> Color -> Shade -> Color
ambColor ls c = const $ grey (float2Double ls) * c

pointLight :: Float -> Color -> V3 Float -> Light
pointLight ls c loc =
    Light False (ptDir ls c loc) (ptColor ls c)

ptDir :: Float -> Color -> V3 Float -> Shade -> V3 Float
ptDir _ _ loc sh = signorm $ loc - (sh^.localHitPoint)

ptColor :: Float -> Color -> Shade -> Color
ptColor ls c = const $ grey (float2Double ls) * c
