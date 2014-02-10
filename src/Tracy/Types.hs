{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances #-}
module Tracy.Types where

import Control.Lens
import Control.Monad.Random
import Data.Time.Clock
import Linear
import Data.Colour

type Color = Colour

data InfoEvent =
      ISampleRoot Float
    | IAccelSchemeName String
    | INumObjects Int
    | IShadows Bool
    | INumSquareSampleSets Int
    | INumDiskSampleSets Int
    | INumCPUs Int
    | INumChunks Int
    | INumRowsPerChunk Int
    | IChunkFinished Int Int
    | IStartTime UTCTime
    | IFinishTime UTCTime
    | ITotalTime NominalDiffTime
    | IImageSize Int Int
    | IStarted
    | IFinished
    | IShutdown
    deriving (Eq, Show)

data DataEvent =
      DNumChunks Int
    | DChunkFinished Int [[Colour]]
    | DImageSize Int Int
    | DStarted
    | DFinished
    | DShutdown
    deriving (Eq, Show)

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
           , _bounding_box :: Maybe BBox
           }

instance Show Object where
    show _ = "Object {...}"

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
          , _worldShadows :: Bool
          }

data TraceState =
    TraceState { _traceRNG :: StdGen
               , _traceConfig :: Config
               , _traceNumSampleSets :: Int
               }

data AccelScheme =
    AccelScheme { _schemeName :: String
                , _schemeApply :: World -> World
                }

data Config =
    Config { _vpSampler :: Sampler (Float, Float)
           , _sampleRoot :: Float
           , _accelScheme :: AccelScheme
           , _cpuCount :: Int
           , _workChunks :: Int
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

data BBox =
    BBox { _bboxP0 :: V3 Float
         , _bboxP1 :: V3 Float
         }
         deriving (Show)

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
makeLenses ''BBox
makeLenses ''AccelScheme
makeLenses ''Config
