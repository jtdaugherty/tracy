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
           , _hit = hitCompound (os^..folded.hit)
           , _shadow_hit = shadowHitCompound (os^..folded.hit)
           , _bounding_box = error "compound has no bounding_box"
           }

hitCompound :: [Ray -> Maybe (Shade, Float)] -> Ray -> Maybe (Shade, Float)
hitCompound hitFuncs r =
    listToMaybe $ sortBy (comparing snd) $ catMaybes results
    where
      results = hitFuncs <*> pure r

shadowHitCompound :: [Ray -> Maybe (Shade, Float)] -> Ray -> Maybe Float
shadowHitCompound hitFuncs r = snd <$> hitCompound hitFuncs r
