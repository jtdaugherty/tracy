module Tracy.Anim
  ( animV3
  , animFloat
  )
  where

import Linear

import Tracy.Types
import Tracy.Util
import Tracy.Transformations

animV3 :: Int -> AnimV3 -> V3 Float
animV3 _ (V3Val v) = v
animV3 fn (V3Lerp (fa, fb) (initial, final))
  | fn < fa = initial
  | fn > fb = final
  | otherwise = let p = ((toEnum $ fn - fa) / (toEnum $ fb - fa)) :: Float
                in ((1 - p) *^ initial) + (p *^ final)
animV3 fn (V3LerpRotY (fa, fb) initialValue finalAngle)
  | fn < fa = initialValue
  | fn > fb = let Trans (tf, _) = rotateY finalAngle
              in toV3 $ tf !* (toV4 initialValue)
  | otherwise = let p = ((toEnum $ fn - fa) / (toEnum $ fb - fa)) :: Float
                    Trans (tf, _) = rotateY (p * finalAngle)
                in toV3 $ tf !* (toV4 initialValue)

animFloat :: Int -> AnimFloat -> Float
animFloat _ (FloatVal v) = v
animFloat fn (FloatLerp (fa, fb) (initial, final))
  | fn < fa = initial
  | fn > fb = final
  | otherwise = let p = ((toEnum $ fn - fa) / (toEnum $ fb - fa)) :: Float
                in ((1 - p) * initial) + (p * final)
