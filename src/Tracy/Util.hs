module Tracy.Util
  ( max3
  , min3
  , getColorBytes
  , defaultShade
  , clamp
  , dSquared
  ) where

import Control.Applicative
import Control.Lens
import Data.Colour
import Linear
import qualified Data.ByteString as B

import Tracy.Types

getColorBytes :: Colour -> B.ByteString
getColorBytes (Colour r g b) =
    B.pack $ (toEnum . fromEnum) <$> [ r * 255, g * 255, b * 255, 255 ]

defaultShade :: Shade
defaultShade =
    Shade { _localHitPoint = error "no local hit point set on Shade"
          , _normal = error "no normal set on Shade"
          , _hitPoint = error "no hit point set on Shade"
          , _material = error "no material set on Shade"
          , _shadeRay = Ray (V3 0 0 0) (V3 0 0 0)
          , _depth = 0
          , _dir = V3 0 0 0
          }

clamp :: (Ord a) => a -> a -> a -> a
clamp v mnB mxB = if v < mnB
                  then mnB else if v > mxB
                                then mxB else v

mx :: (Ord a) => a -> a -> a
mx a b = if a > b then a else b

mn :: (Ord a) => a -> a -> a
mn a b = if a < b then a else b

max3 :: (Ord a) => a -> a -> a -> a
max3 a b c = mx a $ mx b c

min3 :: (Ord a) => a -> a -> a -> a
min3 a b c = mn a $ mn b c

dSquared :: (Floating a, Num a) => V3 a -> V3 a -> a
dSquared v1 v2 = (v1^._x - v2^._x) ** 2 +
                 (v1^._y - v2^._y) ** 2 +
                 (v1^._z - v2^._z) ** 2
