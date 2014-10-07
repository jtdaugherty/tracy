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
    Light sh (ptDir ls c loc) (ptColor ls c) (ptShadow loc)

ptShadow :: V3 Float -> World -> Ray -> Bool
ptShadow loc w r =
    (not . null) $ filter (< d) $ catMaybes results
    where
        results = (w^..objects.folded.shadow_hit) <*> pure r
        d = distance loc (r^.origin)

ptDir :: Float -> Color -> V3 Float -> V3 Float -> Shade -> V3 Float
ptDir _ _ loc _ sh = signorm $ loc - (sh^.localHitPoint)

ptColor :: Float -> Color -> World -> V3 Float -> Shade -> Color
ptColor ls c _ _ = const $ grey (float2Double ls) * c
