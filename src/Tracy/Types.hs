{-# LANGUAGE TemplateHaskell #-}
module Tracy.Types where

import Control.Lens
import Linear
import Data.Colour

type Color = Colour

data Shade =
    Shade { _localHitPoint :: V3 Double
          , _normal :: V3 Double
          , _shadeColor :: Color
          }
    deriving (Show)

data Ray =
    Ray { _origin :: V3 Double
        , _direction :: V3 Double
        }
    deriving (Show)

data Object =
    Object { _objectColor :: Color
           , _hit :: Ray -> Maybe (Shade, Double)
           }

data ViewPlane =
    ViewPlane { _hres :: Double
              , _vres :: Double
              , _pixelSize :: Double
              , _gamma :: Double
              , _inverseGamma :: Double
              }
    deriving (Show)

makeLenses ''Shade
makeLenses ''Ray
makeLenses ''Object
makeLenses ''ViewPlane
