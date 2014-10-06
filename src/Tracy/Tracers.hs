module Tracy.Tracers
  ( basicTracer
  )
  where

import Control.Applicative
import Data.List
import Data.Maybe
import Data.Ord (comparing)

import Tracy.Types

basicTracer :: [Ray -> Maybe (Shade, Float)] -> Ray -> Maybe (Shade, Float)
basicTracer hitFuncs r =
    listToMaybe $ sortBy (comparing snd) $ catMaybes $ hitFuncs <*> pure r
