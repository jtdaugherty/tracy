module Tracy.Textures.PlaneChecker
  ( planeChecker
  )
where

import Control.Lens
import Data.Colour
import Linear
import Tracy.Types

planeChecker :: Double -> Texture
planeChecker = Texture . checkerColor

checkerColor :: Double -> Shade -> Color
checkerColor size sh =
    let x = if hp^._x < 0 then hp^._x - size else hp^._x
        z = if hp^._z < 0 then hp^._z - size else hp^._z
        row = truncate (x / size) :: Integer
        col = truncate (z / size) :: Integer
        hp = sh^.localHitPoint
    in if (row + col) `mod` 2 == 0
       then cBlack
       else cWhite
