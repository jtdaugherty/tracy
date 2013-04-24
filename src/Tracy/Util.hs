module Tracy.Util where

import Control.Applicative
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
          , _shadeRay  = Ray (V3 0 0 0) (V3 0 0 0)
          , _depth = 0
          , _dir = V3 0 0 0
          }
