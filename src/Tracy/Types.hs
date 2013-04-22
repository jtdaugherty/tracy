{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances #-}
module Tracy.Types where

import Control.Lens
import Control.Monad.State
import Control.Monad.Random
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

data World =
    World { _viewPlane :: ViewPlane
          , _bgColor :: Color
          , _objects :: [Object]
          }

data TraceState =
    TraceState { _traceLog :: [String]
               , _traceRNG :: StdGen
               , _traceSampler :: Sampler
               , _traceNumSamples :: Int
               , _traceNumSampleSets :: Int
               }

type TraceM = State TraceState

type Sampler = Int -> Int -> TraceM [[(Double, Double)]]

makeLenses ''Shade
makeLenses ''Ray
makeLenses ''Object
makeLenses ''ViewPlane
makeLenses ''World
makeLenses ''TraceState
