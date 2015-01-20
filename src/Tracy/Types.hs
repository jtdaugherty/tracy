{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances, DeriveGeneric, DefaultSignatures, BangPatterns, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tracy.Types where

import Control.Applicative
import Control.Lens
import Data.Serialize
import Data.Time.Clock
import Control.Monad.Reader
import qualified Data.Vector.Storable as SV
import qualified Data.Vector as V
import GHC.Generics
import Linear
import Data.Colour
import Data.Monoid
import System.Random.MWC
import Foreign.Storable
import Foreign.C.Types
import Foreign.Ptr
import Data.Vector.Cereal ()

type Color = Colour

data InfoEvent =
      ISampleRoot Float
    | IFrameNum Int
    | IConnected String
    | IConnecting String
    | ISceneName String
    | IAccelScheme AccelSchemeDesc
    | INumObjects Int
    | IShadows Bool
    | INumCPUs Int
    | INumBatches Int
    | IBatchFinished Int Int NominalDiffTime
    | IStartTime UTCTime
    | IFinishTime UTCTime
    | ITotalTime NominalDiffTime
    | IImageSize Int Int
    | IStarted
    | IFinished
    | IShutdown
    deriving (Eq)

data DataEvent =
      DSceneName String
    | DNumBatches Int
    | DFrameNum Int
    | DSampleRoot Float
    | DBatchFinished (SV.Vector Colour)
    | DImageSize Int Int
    | DStarted
    | DFinished
    | DShutdown
    deriving (Eq, Show)

data JobRequest =
      SetScene RenderConfig SceneDesc Int
    | RenderRequest
    | RenderFinished
    | Shutdown
    deriving (Generic, Show)

data JobResponse =
      JobError String
    | BatchFinished (SV.Vector Colour)
    | JobAck
    deriving (Generic)

class Anim a b where
    animate :: Int -> a -> b

data AnimV3 =
      V3Val (V3 Float)
    | V3Lerp (Int, Int) (V3 Float, V3 Float)
    | V3LerpRotY (Int, Int) (V3 Float) Float
    deriving (Generic, Eq, Show)

data AnimFloat =
      FloatVal Float
    | FloatLerp (Int, Int) (Float, Float)
    deriving (Generic, Eq, Show)

-- A transformation is a pair of (forward transformation matrix, inverse
-- transformation matrix).  Note the Monoid instance for this type.
data Transformation = Trans (M44 Float, M44 Float)
    deriving (Eq, Read, Show, Generic)

instance Monoid Transformation where
    mempty = Trans (eye4, eye4)
    (Trans (f1, i1)) `mappend` (Trans (f2, i2)) =
        Trans (f1 !*! f2, i2 !*! i1)

data Shade =
    Shade { _localHitPoint :: V3 Float
          , _normal :: V3 Float
          , _material :: Material
          , _shadeRay :: Ray
          , _depth :: Int
          }

data Ray =
    Ray { _origin :: !(V3 Float)
        , _direction :: !(V3 Float)
        }
    deriving (Show)

data ObjectAreaLightImpl =
    ObjectALI { _objectSurfaceSample :: TraceM (V3 Float)
              , _objectGetNormal :: V3 Float -> V3 Float
              , _objectPDF :: LightDir -> Shade -> Float
              }

data Object =
    Object { _objectMaterial :: Material
           , _hit :: Ray -> Maybe (Shade, Float)
           , _shadow_hit :: Ray -> Maybe Float
           , _bounding_box :: Maybe BBox
           , _areaLightImpl :: Maybe ObjectAreaLightImpl
           }

instance Show Object where
    show _ = "Object {...}"

data ViewPlane =
    ViewPlane { _hres :: Float
              , _vres :: Float
              , _pixelSize :: Float
              , _gamma :: Float
              , _inverseGamma :: Float
              , _maxDepth :: Int
              }
    deriving (Show, Eq, Generic)

data World =
    World { _viewPlane :: ViewPlane
          , _bgColor :: Color
          , _objects :: [Object]
          , _lights :: [Light]
          , _ambient :: Light
          , _worldShadows :: Bool
          }

data Scene a =
    Scene { _sceneWorld :: World
          , _sceneAccelScheme :: AccelScheme
          , _sceneCamera :: Camera a
          , _sceneTracer :: Tracer
          }

data AccelScheme =
    AccelScheme { _schemeName :: String
                , _schemeApply :: World -> World
                }

data RenderConfig =
    RenderConfig { _sampleRoot :: Float
                 , _forceShadows :: Maybe Bool
                 }
                 deriving (Generic, Show)

data BRDF =
    BRDF { _brdfFunction :: Shade -> V3 Float -> V3 Float -> Color
         , _brdfSampleF :: Shade -> V3 Float -> TraceM (Float, Color, V3 Float)
         , _brdfRho ::  Shade -> V3 Float -> Color
         }

data LightDir =
    LD { _lightDir :: V3 Float
       , _lightSamplePoint :: V3 Float
       , _lightNormal :: V3 Float
       }

data Light =
    Light { _lightShadows :: Bool
          , _lightDirection :: Shade -> TraceM LightDir
          , _lightColor :: LightDir -> Shade -> TraceM Color
          , _inLightShadow :: LightDir -> Ray -> TraceM Bool
          , _lightG :: LightDir -> Shade -> Float
          , _lightPDF :: LightDir -> Shade -> Float
          }

data Material =
    Material { _doShading :: Shade -> Tracer -> TraceM Color
             , _doAreaShading :: Shade -> Tracer -> TraceM Color
             , _doPathShading :: Shade -> Tracer -> TraceM Color
             , _getLe :: Shade -> Color
             }

data BBox =
    BBox { _bboxP0 :: !(V3 Float)
         , _bboxP1 :: !(V3 Float)
         }
         deriving (Show)

data Sampler a = Sampler (GenIO -> Float -> IO (V.Vector a))

instance Functor Sampler where
    fmap f (Sampler g) = Sampler g'
      where
      g' gen root = do
          vs <- g gen root
          return $ f <$> vs

data SampleData =
    SampleData { _numSets :: Int
               , _squareSampleSets :: V.Vector (V.Vector (Float, Float))
               , _diskSampleSets :: V.Vector (V.Vector (Float, Float))
               , _objectSampleSets :: V.Vector (V.Vector (Float, Float))
               }

type CameraRenderer a = Camera a
                      -> RenderConfig
                      -> World
                      -> Tracer
                      -> SampleData
                      -> (Int, V.Vector Int)
                      -> SV.Vector Color

data Camera a =
    Camera { _cameraU :: V3 Float
           , _cameraV :: V3 Float
           , _cameraW :: V3 Float
           , _cameraRenderWorld :: CameraRenderer a
           , _cameraData :: a
           , _exposureTime :: Float
           , _cameraZoomFactor :: Float
           , _cameraEyePoint :: V3 Float
           }

data ThinLens =
    ThinLens { _lensRadius :: Float
             , _lensVPDistance :: Float
             , _lensFocalPlaneDistance :: Float
             , _lensRayDir :: Camera ThinLens -> V2 Float -> V2 Float -> V3 Float
             , _lensSampler :: Sampler (Float, Float)
             }

data V2SamplerDesc = Regular
                   | PureRandom
                   | Jittered
                   | MultiJittered
                   | CorrelatedMultiJittered
                   | UnitDisk V2SamplerDesc
                     deriving (Show, Eq, Generic)

data V3SamplerDesc = UnitHemi Float V2SamplerDesc
                     deriving (Show, Eq, Generic)

data TraceData =
    TD { _tdHemiSample :: V3 Float
       , _tdHemiSampleExp :: Float -> V3 Float
       , _tdDiskSample :: V2 Float
       , _tdSquareSample :: V2 Float
       , _tdObjectSurfaceSample :: V2 Float
       , _tdWorld :: World
       , _tdWorldHitFuncs :: [Ray -> Maybe (Shade, Float)]
       , _tdWorldShadowHitFuncs :: [Ray -> Maybe Float]
       }

type TraceM a = Reader TraceData a

data Tracer =
    Tracer { _doTrace :: Ray -> Int -> TraceM Color
           }

---------------------------------------------------------------------------
-- World description types for transmission between rendering nodes
---------------------------------------------------------------------------
data TracerDesc =
    RayCastTracer
  | AreaLightTracer
  | WhittedTracer
  | PathTracer
    deriving (Eq, Show, Generic)

data SceneDesc =
    SceneDesc { _sceneDescWorld :: WorldDesc
              , _sceneDescAccelScheme :: AccelSchemeDesc
              , _sceneDescCamera :: CameraDesc
              , _sceneDescTracer :: TracerDesc
              }
    deriving (Eq, Show, Generic)

data AccelSchemeDesc =
      NoScheme
    | GridScheme
    deriving (Eq, Show, Generic)

data WorldDesc =
    WorldDesc { _wdViewPlane :: ViewPlane
              , _wdBgColor :: Color
              , _wdObjects :: [ObjectDesc]
              , _wdLights :: [LightDesc]
              , _wdAmbient :: LightDesc
              , _wdWorldShadows :: Bool
              }
    deriving (Eq, Show, Generic)

data MeshDesc =
    MeshDesc { meshDescVertices :: V.Vector (V3 Float, V3 Float)
             , meshDescFaces :: [V.Vector Int]
             }
    deriving (Eq, Show)

data ObjectDesc =
      Sphere (V3 Float) Float MaterialDesc
    | ConcaveSphere (V3 Float) Float MaterialDesc
    | Rectangle (V3 Float) (V3 Float) (V3 Float) MaterialDesc
    | Triangle (V3 Float) (V3 Float) (V3 Float) MaterialDesc
    | Box (V3 Float) (V3 Float) MaterialDesc
    | Plane (V3 Float) (V3 Float) MaterialDesc
    | Mesh MeshDesc MaterialDesc
    | Instances ObjectDesc [(Transformation, Maybe MaterialDesc)]
    | Grid [ObjectDesc]
    deriving (Eq, Show, Generic)

data LightDesc =
      Ambient Float Color
    | AmbientOccluder Color Color Float
    | Point Bool Float Color (V3 Float)
    | Area Bool ObjectDesc (Maybe Float)
    | Environment Bool MaterialDesc
    deriving (Eq, Show, Generic)

data MaterialDesc =
      Matte Color
    | Phong Color Float Float
    | Emissive Color Float
    | Reflective Color Float Float Color Float
    | GlossyReflective Color Float Float Color Float Float
    deriving (Eq, Show, Generic)

data CameraDesc =
    ThinLensCamera { _thinLensEye :: AnimV3
                   , _thinLensLookAt :: V3 Float
                   , _thinLensUp :: V3 Float
                   , _thinLensExposure :: Float
                   , _thinLensZ :: Float
                   , _thinLensVpDist :: Float
                   , _thinLensFpDist :: Float
                   , _thinLensRadius :: AnimFloat
                   , _thinLensSampler :: V2SamplerDesc
                   }
    deriving (Eq, Show, Generic)

instance Serialize a => Serialize (V3 a) where
    get = V3 <$> get <*> get <*> get
    put (V3 x y z) = put x >> put y >> put z

instance Serialize a => Serialize (V4 a) where
    get = V4 <$> get <*> get <*> get <*> get
    put (V4 x y z w) = put x >> put y >> put z >> put w

instance Serialize Colour where
    get = Colour <$> get <*> get <*> get
    put (Colour r g b) = put r >> put g >> put b

instance Serialize Colour8 where
    get = Colour8 <$> get <*> get <*> get
    put (Colour8 r g b) = put r >> put g >> put b

instance Storable Colour where
    sizeOf _ = sizeOf (undefined :: CDouble) * 3
    alignment _ = alignment (undefined :: CDouble)

    {-# INLINE peek #-}
    peek p = do
               r <- peekElemOff q 0
               g <- peekElemOff q 1
               b <- peekElemOff q 2
               return (Colour r g b)
      where
        q = castPtr p
    {-# INLINE poke #-}
    poke p (Colour r g b) = do
               pokeElemOff q 0 r
               pokeElemOff q 1 g
               pokeElemOff q 2 b
      where
        q = castPtr p

instance Serialize SceneDesc where
instance Serialize WorldDesc where
instance Serialize V2SamplerDesc where
instance Serialize V3SamplerDesc where
instance Serialize CameraDesc where
instance Serialize ObjectDesc where
instance Serialize LightDesc where
instance Serialize MaterialDesc where
instance Serialize AccelSchemeDesc where
instance Serialize TracerDesc where
instance Serialize ViewPlane where
instance Serialize JobRequest where
instance Serialize JobResponse where
instance Serialize RenderConfig where
instance Serialize Transformation where
instance Serialize AnimV3 where
instance Serialize AnimFloat where

instance Serialize MeshDesc where
    get = MeshDesc <$> (V.fromList <$> get) <*> get
    put (MeshDesc vs fs) = put (V.toList vs) >> put fs

makeLenses ''Shade
makeLenses ''Ray
makeLenses ''Object
makeLenses ''ViewPlane
makeLenses ''World
makeLenses ''BRDF
makeLenses ''Light
makeLenses ''Material
makeLenses ''BBox
makeLenses ''AccelScheme
makeLenses ''RenderConfig
makeLenses ''Scene
makeLenses ''Camera
makeLenses ''ThinLens
makeLenses ''Tracer
makeLenses ''TraceData
makeLenses ''LightDir
makeLenses ''ObjectAreaLightImpl
makeLenses ''SampleData

makeLenses ''SceneDesc
makeLenses ''WorldDesc
makeLenses ''CameraDesc
