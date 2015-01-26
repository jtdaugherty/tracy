module Tracy.Objects.Compound
  ( compound
  )
  where

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
           , _bounding_box = Nothing
           , _areaLightImpl = Nothing
           }

hitCompound :: [Ray -> Maybe (Shade, Double)] -> Ray -> Maybe (Shade, Double)
hitCompound hitFuncs r =
    listToMaybe $ sortBy (comparing snd) $ catMaybes results
    where
      results = hitFuncs <*> pure r

shadowHitCompound :: [Ray -> Maybe (Shade, Double)] -> Ray -> Maybe Double
shadowHitCompound hitFuncs r = snd <$> hitCompound hitFuncs r
