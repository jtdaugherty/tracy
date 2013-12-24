module Tracy.Compound where

import Tracy.Types
import Data.Maybe
import Control.Lens
import Control.Applicative
import Data.List
import Data.Ord (comparing)

compound :: [Object] -> Material -> Object
compound [] _ = error "empty compound"
compound [o] _ = o
compound os m =
    Object { _objectMaterial = m -- XXX unused
           , _hit = hitCompound os
           , _shadow_hit = shadowHitCompound os
           , _bounding_box = error "compound has no bounding_box"
           }

hitCompound :: [Object] -> Ray -> Maybe (Shade, Float)
hitCompound os r =
    listToMaybe $ sortBy (comparing snd) $ catMaybes results
    where
      results = tests <*> pure r
      tests = os^..folded.hit

shadowHitCompound :: [Object] -> Ray -> Maybe Float
shadowHitCompound os r = snd <$> hitCompound os r
