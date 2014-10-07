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

ptShadow :: V3 Float -> Ray -> TraceM Bool
ptShadow loc r = do
    w <- view tdWorld
    let results = (w^..objects.folded.shadow_hit) <*> pure r
        d = distance loc (r^.origin)
    return $ (not . null) $ filter (< d) $ catMaybes results

ptDir :: Float -> Color -> V3 Float -> Shade -> TraceM (V3 Float)
ptDir _ _ loc sh = return $ signorm $ loc - (sh^.localHitPoint)

ptColor :: Float -> Color -> Shade -> TraceM Color
ptColor ls c = const $ return $ grey (float2Double ls) * c
