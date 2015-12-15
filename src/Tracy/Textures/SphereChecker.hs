module Tracy.Textures.SphereChecker
  ( sphereChecker
  )
where

import Control.Lens
import Data.Colour
import Linear
import Tracy.Types
import Tracy.Constants

sphereChecker :: Double -> Texture
sphereChecker = Texture . checkerColor

checkerColor :: Double -> Shade -> Color
checkerColor count sh =
    let hp = signorm $ sh^.localHitPoint
        theta = acos (hp^._y)
        phi' = atan2 (hp^._x) (hp^._z)
        phi = if phi' < 0
              then phi' + (2 * pi)
              else phi'
        u = phi * invTWOPI
        v = 1 - theta * invPI
        row = truncate (u * count) :: Integer
        col = truncate (v * count) :: Integer
    in if (row + col) `mod` 2 == 0
       then cBlack
       else cWhite
