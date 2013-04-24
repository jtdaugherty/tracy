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
          , _material :: Material
          , _hitPoint :: V3 Double
          , _shadeRay :: Ray
          , _depth :: Double
          , _dir :: V3 Double
          }

data Ray =
    Ray { _origin :: V3 Double
        , _direction :: V3 Double
        }
    deriving (Show)

data Object =
    Object { _objectMaterial :: Material
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
          , _lights :: [Light]
          , _ambient :: Light
          }

data TraceState =
    TraceState { _traceRNG :: StdGen
               , _traceConfig :: Config
               , _traceNumSampleSets :: Int
               }

data Config =
    Config { vpSampler :: Sampler (Double, Double)
           , sampleRoot :: Double
           , numThreads :: Int
           }

data BRDF =
    BRDF { _brdfFunction :: BRDFData -> Shade -> V3 Double -> V3 Double -> Color
         , _brdfSampleF :: BRDFData -> Shade -> V3 Double -> V3 Double -> V3 Double -> (Double, Color)
         , _brdfRho :: BRDFData -> Shade -> V3 Double -> Color
         , _brdfSampler :: Sampler (V3 Double)
         , _brdfData :: BRDFData
         }

data BRDFData =
    BRDFData { _brdfKD :: Double
             , _brdfColor :: Color
             }

data Light =
    Light { _lightShadows :: Bool
          , _lightDirection :: Shade -> V3 Double
          , _lightColor :: Shade -> Color
          }

data Material =
    Material { _doShading :: World -> Shade -> Color
             }

type TraceM = State TraceState

type Sampler a = Double -> Int -> TraceM [[a]]

makeLenses ''Shade
makeLenses ''Ray
makeLenses ''Object
makeLenses ''ViewPlane
makeLenses ''World
makeLenses ''TraceState
makeLenses ''BRDF
makeLenses ''BRDFData
makeLenses ''Light
makeLenses ''Material
