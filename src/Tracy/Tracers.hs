module Tracy.Tracers
  ( basicTracer
  )
  where

import Control.Applicative
import Control.Lens ((^.), (&), (.~))
import Data.List
import Data.Maybe
import Data.Ord (comparing)
import Linear (V3)

import Tracy.Types

basicTracer :: [Ray -> Maybe (Shade, Float)]
            -> V3 Float
            -> World
            -> Ray
            -> Color
basicTracer hitFuncs hemiCoords w ray =
    case doHit hitFuncs ray of
        Nothing -> w^.bgColor
        Just (sh, _t) -> (sh^.material.doShading) hemiCoords (w^.worldShadows) w (sh & shadeRay .~ ray)

doHit :: [Ray -> Maybe (Shade, Float)] -> Ray -> Maybe (Shade, Float)
doHit hitFuncs r =
    listToMaybe $ sortBy (comparing snd) $ catMaybes $ hitFuncs <*> pure r
