module Tracy.Anim
  ( animV3
  )
  where

import Linear

import Tracy.Types
import Tracy.Util
import Tracy.Transformations

animV3 :: Int -> AnimV3 -> V3 Float
animV3 _ (Val v) = v
animV3 fn (Lerp (fa, fb) (initial, final))
  | fn < fa = initial
  | fn > fb = final
  | otherwise = let p = ((toEnum $ fn - fa) / (toEnum $ fb - fa)) :: Float
                in ((1 - p) *^ initial) + (p *^ final)
animV3 fn (LerpRotY (fa, fb) initialValue finalAngle)
  | fn < fa = initialValue
  | fn > fb = let Trans (tf, _) = rotateY finalAngle
              in toV3 $ tf !* (toV4 initialValue)
  | otherwise = let p = ((toEnum $ fn - fa) / (toEnum $ fb - fa)) :: Float
                    Trans (tf, _) = rotateY (p * finalAngle)
                in toV3 $ tf !* (toV4 initialValue)

