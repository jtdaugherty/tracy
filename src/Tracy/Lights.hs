module Tracy.Lights where

import Control.Lens
import Linear
import Data.Colour

import Tracy.Types

ambientLight :: Double -> Color -> Light
ambientLight ls c =
    Light False ambDir (ambColor ls c)

ambDir :: Shade -> V3 Double
ambDir = const $ V3 0 0 0

ambColor :: Double -> Color -> Shade -> Color
ambColor ls c = const $ grey ls * c

pointLight :: Double -> Color -> V3 Double -> Light
pointLight ls c loc =
    Light False (ptDir ls c loc) (ptColor ls c)

ptDir :: Double -> Color -> V3 Double -> Shade -> V3 Double
ptDir _ _ loc sh = signorm $ loc - (sh^.localHitPoint)

ptColor :: Double -> Color -> Shade -> Color
ptColor ls c = const $ grey ls * c
