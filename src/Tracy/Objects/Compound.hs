module Tracy.Objects.Compound
  ( compound
  )
  where

import Tracy.Types
import Data.Maybe
import Control.Lens
import Control.Applicative
import Data.Ord (comparing)
import qualified Data.Vector as V

compound :: V.Vector Object -> Material -> Object
compound os m
  | V.null os = error "empty compound"
  | V.length os == 1 = os V.! 0
  | otherwise =
    Object { _objectMaterial = m -- XXX unused
           , _hit = hitCompound $ V.map (^.hit) os
           , _shadow_hit = shadowHitCompound $ V.map (^.hit) os
           , _bounding_box = Nothing
           , _areaLightImpl = Nothing
           }

hitCompound :: V.Vector (Ray -> Maybe (Shade, Double)) -> Ray -> Maybe (Shade, Double)
hitCompound hitFuncs r =
    if V.null hits
    then Nothing
    else Just $ V.minimumBy (comparing snd) hits
    where
        results :: V.Vector (Maybe (Shade, Double))
        results = V.map ($ r) hitFuncs
        hits :: V.Vector (Shade, Double)
        hits = V.map fromJust $ V.filter isJust results

shadowHitCompound :: V.Vector (Ray -> Maybe (Shade, Double)) -> Ray -> Maybe Double
shadowHitCompound hitFuncs r = snd <$> hitCompound hitFuncs r
