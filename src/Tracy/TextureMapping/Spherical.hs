module Tracy.TextureMapping.Spherical
  ( sphericalMapping
  )
where

import Control.Lens
import Linear
import Tracy.Types
import Tracy.Constants

sphericalMapping :: TextureMapping
sphericalMapping = TextureMapping doMapping

doMapping :: V3 Double -> Int -> Int -> (Int, Int)
doMapping hp' imgHres imgVres =
    let hp = signorm hp'
        theta = acos (hp^._y)
        phi' = atan2 (hp^._x) (hp^._z)
        phi = if phi' < 0
              then phi' + (2 * pi)
              else phi'
        u = phi * invTWOPI
        v = 1 - theta * invPI
        row = truncate $ (toEnum imgVres - 1) * v
        col = truncate $ (toEnum imgHres - 1) * u
    in (row, col)
