module Tracy.Transformations
  ( translate
  , scale
  , scaleUni
  , rotateX
  , rotateY
  , rotateZ
  ) where

import Data.Distributive
import Linear

import Tracy.Types

translate :: Double -> Double -> Double -> Transformation
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

scaleUni :: Double -> Transformation
scaleUni f = scale f f f

scale :: Double -> Double -> Double -> Transformation
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

rotateX :: Double -> Transformation
rotateX a = Trans (rotX a, distribute $ rotX a)

rotateY :: Double -> Transformation
rotateY a = Trans (rotY a, distribute $ rotY a)

rotateZ :: Double -> Transformation
rotateZ a = Trans (rotZ a, distribute $ rotZ a)

rotX :: Double -> V4 (V4 Double)
rotX a = V4 r1 r2 r3 r4
  where
    r1 = V4 1 0 0 0
    r2 = V4 0 (cos a) (-1 * sin a) 0
    r3 = V4 0 (sin a) (cos a) 0
    r4 = V4 0 0 0 1

rotY :: Double -> V4 (V4 Double)
rotY a = V4 r1 r2 r3 r4
  where
    r1 = V4 (cos a) 0 (sin a) 0
    r2 = V4 0 1 0 0
    r3 = V4 (-1 * sin a) 0 (cos a) 0
    r4 = V4 0 0 0 1

rotZ :: Double -> V4 (V4 Double)
rotZ a = V4 r1 r2 r3 r4
  where
    r1 = V4 (cos a) (-1 * sin a) 0 0
    r2 = V4 (sin a) (cos a) 0 0
    r3 = V4 0 0 1 0
    r4 = V4 0 0 0 1
