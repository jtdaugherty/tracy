{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Tracy.Anim () where

import Linear

import Tracy.Types
import Tracy.Util
import Tracy.Transformations

instance Anim AnimV3 (V3 Double) where
    animate = animV3

instance Anim AnimDouble Double where
    animate = animDouble

animV3 :: Frame -> AnimV3 -> V3 Double
animV3 _ (V3Val v) = v
animV3 (Frame fn) (V3Lerp (Frame fa, Frame fb) (initial, final))
  | fn < fa = initial
  | fn > fb = final
  | otherwise = let p = ((toEnum $ fn - fa) / (toEnum $ fb - fa)) :: Double
                in ((1 - p) *^ initial) + (p *^ final)
animV3 (Frame fn) (V3LerpRotY (Frame fa, Frame fb) initialValue finalAngle)
  | fn < fa = initialValue
  | fn > fb = let Trans (tf, _) = rotateY finalAngle
              in toV3 $ tf !* (toV4 initialValue)
  | otherwise = let p = ((toEnum $ fn - fa) / (toEnum $ fb - fa)) :: Double
                    Trans (tf, _) = rotateY (p * finalAngle)
                in toV3 $ tf !* (toV4 initialValue)

animDouble :: Frame -> AnimDouble -> Double
animDouble _ (DoubleVal v) = v
animDouble (Frame fn) (DoubleLerp (Frame fa, Frame fb) (initial, final))
  | fn < fa = initial
  | fn > fb = final
  | otherwise = let p = ((toEnum $ fn - fa) / (toEnum $ fb - fa)) :: Double
                in ((1 - p) * initial) + (p * final)
