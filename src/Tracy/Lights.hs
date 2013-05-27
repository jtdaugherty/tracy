module Tracy.Lights where

import Control.Applicative
import Control.Lens
import Linear
import Data.Colour
import Data.Maybe
import GHC.Float

import Tracy.Types

ambientLight :: Float -> Color -> Light
ambientLight ls c =
    Light False ambDir (ambColor ls c) ambShadow

ambDir :: Shade -> V3 Float
ambDir = const $ V3 0 0 0

ambColor :: Float -> Color -> Shade -> Color
ambColor ls c = const $ grey (float2Double ls) * c

ambShadow :: World -> Ray -> Bool
ambShadow _ _ = False

pointLight :: Bool -> Float -> Color -> V3 Float -> Light
pointLight sh ls c loc =
    Light sh (ptDir ls c loc) (ptColor ls c) (ptShadow loc)

ptShadow :: V3 Float -> World -> Ray -> Bool
ptShadow loc w r =
    (not . null) $ filter (< d) $ catMaybes results
    where
        results = (w^..objects.folded.shadow_hit) <*> pure r
        d = distance loc (r^.origin)

ptDir :: Float -> Color -> V3 Float -> Shade -> V3 Float
ptDir _ _ loc sh = signorm $ loc - (sh^.localHitPoint)

ptColor :: Float -> Color -> Shade -> Color
ptColor ls c = const $ grey (float2Double ls) * c
