module Tracy.Transformations
  ( translate
  , scale
  , scaleUni
  , rotateX
  ) where

import Data.Distributive
import Linear

import Tracy.Types

translate :: Float -> Float -> Float -> Transformation
translate x y z =
    Trans ( V4 r1 r2 r3 r4
          , V4 r1' r2' r3' r4
          )
    where
      r1 = V4 1 0 0 x
      r2 = V4 0 1 0 y
      r3 = V4 0 0 1 z
      r4 = V4 0 0 0 1
      r1' = V4 1 0 0 (-x)
      r2' = V4 0 1 0 (-y)
      r3' = V4 0 0 1 (-z)

scaleUni :: Float -> Transformation
scaleUni f = scale f f f

scale :: Float -> Float -> Float -> Transformation
scale x y z =
    Trans ( V4 r1 r2 r3 r4
          , V4 r1' r2' r3' r4
          )
    where
      r1 = V4 x 0 0 0
      r2 = V4 0 y 0 0
      r3 = V4 0 0 z 0
      r4 = V4 0 0 0 1
      r1' = V4 (1/x) 0 0 0
      r2' = V4 0 (1/y) 0 0
      r3' = V4 0 0 (1/z) 0

rotateX :: Float -> Transformation
rotateX a = Trans (rot a, distribute $ rot a)

rot :: Float -> V4 (V4 Float)
rot a = V4 r1 r2 r3 r4
  where
    r1 = V4 1 0 0 0
    r2 = V4 0 (cos a) (-1 * sin a) 0
    r3 = V4 0 (sin a) (cos a) 0
    r4 = V4 0 0 0 1
