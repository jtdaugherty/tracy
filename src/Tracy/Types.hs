{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances #-}
module Tracy.Types where

import Control.Lens
import Control.Monad.Random
import Linear
import Data.Colour

type Color = Colour

data Shade =
    Shade { _localHitPoint :: V3 Float
          , _normal :: V3 Float
          , _material :: Material
          , _hitPoint :: V3 Float
          , _shadeRay :: Ray
          , _depth :: Float
          , _dir :: V3 Float
          }

data Ray =
    Ray { _origin :: V3 Float
        , _direction :: V3 Float
        }
    deriving (Show)

data Object =
    Object { _objectMaterial :: Material
           , _hit :: Ray -> Maybe (Shade, Float)
           , _shadow_hit :: Ray -> Maybe Float
           }

data ViewPlane =
    ViewPlane { _hres :: Float
              , _vres :: Float
              , _pixelSize :: Float
              , _gamma :: Float
              , _inverseGamma :: Float
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
    Config { vpSampler :: Sampler (Float, Float)
           , sampleRoot :: Float
           , shadows :: Bool
           }

data BRDF =
    BRDF { _brdfFunction :: BRDFData -> Shade -> V3 Float -> V3 Float -> Color
         , _brdfSampleF :: BRDFData -> Shade -> V3 Float -> V3 Float -> V3 Float -> (Float, Color)
         , _brdfRho :: BRDFData -> Shade -> V3 Float -> Color
         , _brdfSampler :: Sampler (V3 Float)
         , _brdfData :: BRDFData
         }

data BRDFData =
    BRDFData { _brdfKD :: Float
             , _brdfColor :: Color
             }

data Light =
    Light { _lightShadows :: Bool
          , _lightDirection :: Shade -> V3 Float
          , _lightColor :: Shade -> Color
          , _inLightShadow :: World -> Ray -> Bool
          }

data Material =
    Material { _doShading :: Bool -> World -> Shade -> Color
             }

type Sampler a = Float -> IO [a]

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
