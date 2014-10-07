module Tracy.AccelSchemes
  ( noScheme
  , gridScheme
  , accelSchemes
  )
  where

import Control.Lens
import Data.Maybe

import Tracy.Types
import Tracy.Objects.Grid

noScheme :: AccelScheme
noScheme = AccelScheme "none" id

gridScheme :: AccelScheme
gridScheme = AccelScheme "grid" applyGrid

applyGrid :: World -> World
applyGrid w =
    let gObjs = [o | o <- _objects w, isJust $ o^.bounding_box ]
        objs = [o | o <- _objects w, not $ isJust $ o^.bounding_box ]
    in w { _objects = grid gObjs:objs }

accelSchemes :: [AccelScheme]
accelSchemes =
    [ noScheme
    , gridScheme
    ]

