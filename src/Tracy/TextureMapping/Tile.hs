module Tracy.TextureMapping.Tile
  ( tileMapping
  )
where

import Control.Lens
import Linear
import Tracy.Types

tileMapping :: Double -> TextureMapping
tileMapping = TextureMapping . doMapping

doMapping :: Double -> V3 Double -> Int -> Int -> (Int, Int)
doMapping scale hp imgHres imgVres =
    let row = (truncate $ scale * hp^._x) `mod` imgHres
        col = (truncate $ scale * hp^._z) `mod` imgVres
    in (row, col)
