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
      ISampleRoot Double
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
    | DSampleRoot Double
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
      V3Val (V3 Double)
    | V3Lerp (Int, Int) (V3 Double, V3 Double)
    | V3LerpRotY (Int, Int) (V3 Double) Double
    deriving (Generic, Eq, Show)

data AnimDouble =
      DoubleVal Double
    | DoubleLerp (Int, Int) (Double, Double)
    deriving (Generic, Eq, Show)

-- A transformation is a pair of (forward transformation matrix, inverse
-- transformation matrix).  Note the Monoid instance for this type.
data Transformation = Trans !(M44 Double, M44 Double)
    deriving (Eq, Read, Show, Generic)

instance Monoid Transformation where
    mempty = Trans (eye4, eye4)
    (Trans (f1, i1)) `mappend` (Trans (f2, i2)) =
        Trans (f1 !*! f2, i2 !*! i1)

data Shade =
    Shade { _localHitPoint :: !(V3 Double)
          , _normal :: !(V3 Double)
          , _material :: Material
          , _shadeRay :: !Ray
          , _depth :: !Int
          }

data Ray =
    Ray { _origin :: !(V3 Double)
        , _direction :: !(V3 Double)
        }
    deriving (Show)

data ObjectAreaLightImpl =
    ObjectALI { _objectSurfaceSample :: TraceM (V3 Double)
              , _objectGetNormal :: V3 Double -> V3 Double
              , _objectPDF :: LightDir -> Shade -> Double
              }

data Object =
    Object { _objectMaterial :: Material
           , _hit :: Ray -> Maybe (Shade, Double)
           , _shadow_hit :: Ray -> Maybe Double
           , _bounding_box :: !(Maybe BBox)
           , _areaLightImpl :: Maybe ObjectAreaLightImpl
           }

instance Show Object where
    show _ = "Object {...}"

data ViewPlane =
    ViewPlane { _hres :: Double
              , _vres :: Double
              , _pixelSize :: Double
              , _gamma :: Double
              , _inverseGamma :: Double
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
    RenderConfig { _sampleRoot :: Double
                 , _forceShadows :: Maybe Bool
                 }
                 deriving (Generic, Show)

data BRDF =
    BRDF { _brdfFunction :: Shade -> V3 Double -> V3 Double -> Color
         , _brdfSampleF :: Shade -> V3 Double -> TraceM (Double, Color, V3 Double)
         , _brdfRho ::  Shade -> V3 Double -> Color
         }

data LightDir =
    LD { _lightDir :: V3 Double
       , _lightSamplePoint :: V3 Double
       , _lightNormal :: V3 Double
       }

data Light =
    Light { _lightShadows :: Bool
          , _lightDirection :: Shade -> TraceM LightDir
          , _lightColor :: LightDir -> Shade -> TraceM Color
          , _inLightShadow :: LightDir -> Ray -> TraceM Bool
          , _lightG :: LightDir -> Shade -> Double
          , _lightPDF :: LightDir -> Shade -> Double
          }

data Material =
    Material { _doShading :: Shade -> Tracer -> TraceM Color
             , _doAreaShading :: Shade -> Tracer -> TraceM Color
             , _doPathShading :: Shade -> Tracer -> TraceM Color
             , _getLe :: Shade -> Color
             }

data BBox =
    BBox { _bboxP0 :: !(V3 Double)
         , _bboxP1 :: !(V3 Double)
         }
         deriving (Show)

data Sampler a = Sampler (GenIO -> Double -> IO (V.Vector a))

instance Functor Sampler where
    fmap f (Sampler g) = Sampler g'
      where
      g' gen root = do
          vs <- g gen root
          return $ f <$> vs

data SampleData =
    SampleData { _numSets :: Int
               , _squareSampleSets :: V.Vector (V.Vector (Double, Double))
               , _diskSampleSets :: V.Vector (V.Vector (Double, Double))
               , _objectSampleSets :: V.Vector (V.Vector (Double, Double))
               }

type CameraRenderer a = Camera a
                      -> RenderConfig
                      -> World
                      -> Tracer
                      -> SampleData
                      -> (Int, V.Vector Int)
                      -> SV.Vector Color

data Camera a =
    Camera { _cameraU :: V3 Double
           , _cameraV :: V3 Double
           , _cameraW :: V3 Double
           , _cameraRenderWorld :: CameraRenderer a
           , _cameraData :: a
           , _exposureTime :: Double
           , _cameraZoomFactor :: Double
           , _cameraEyePoint :: V3 Double
           }

data ThinLens =
    ThinLens { _lensRadius :: Double
             , _lensVPDistance :: Double
             , _lensFocalPlaneDistance :: Double
             , _lensRayDir :: Camera ThinLens -> V2 Double -> V2 Double -> V3 Double
             , _lensSampler :: Sampler (Double, Double)
             }

data V2SamplerDesc = Regular
                   | PureRandom
                   | Jittered
                   | MultiJittered
                   | CorrelatedMultiJittered
                   | UnitDisk V2SamplerDesc
                     deriving (Show, Eq, Generic)

data V3SamplerDesc = UnitHemi Double V2SamplerDesc
                     deriving (Show, Eq, Generic)

data TraceData =
    TD { _tdHemiSample :: !(V3 Double)
       , _tdHemiSampleExp :: Double -> V3 Double
       , _tdDiskSample :: !(V2 Double)
       , _tdSquareSample :: !(V2 Double)
       , _tdObjectSurfaceSample :: !(V2 Double)
       , _tdWorld :: World
       , _tdWorldHitFuncs :: [Ray -> Maybe (Shade, Double)]
       , _tdWorldShadowHitFuncs :: [Ray -> Maybe Double]
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
    MeshDesc { meshDescVertices :: V.Vector (V3 Double, V3 Double)
             , meshDescFaces :: [V.Vector Int]
             }
    deriving (Eq, Show)

data ObjectDesc =
      Sphere (V3 Double) Double MaterialDesc
    | ConcaveSphere (V3 Double) Double MaterialDesc
    | Rectangle (V3 Double) (V3 Double) (V3 Double) MaterialDesc
    | Triangle (V3 Double) (V3 Double) (V3 Double) MaterialDesc
    | Box (V3 Double) (V3 Double) MaterialDesc
    | Plane (V3 Double) (V3 Double) MaterialDesc
    | Mesh MeshDesc MaterialDesc
    | Instances ObjectDesc [(Transformation, Maybe MaterialDesc)]
    | Grid [ObjectDesc]
    deriving (Eq, Show, Generic)

data LightDesc =
      Ambient Double Color
    | AmbientOccluder Color Color Double
    | Point Bool Double Color (V3 Double)
    | Area Bool ObjectDesc (Maybe Double)
    | Environment Bool MaterialDesc
    deriving (Eq, Show, Generic)

data MaterialDesc =
      Matte Color
    | Phong Color Double Double
    | Emissive Color Double
    | Reflective Color Double Double Color Double
    | GlossyReflective Color Double Double Color Double Double
    deriving (Eq, Show, Generic)

data CameraDesc =
    ThinLensCamera { _thinLensEye :: AnimV3
                   , _thinLensLookAt :: V3 Double
                   , _thinLensUp :: V3 Double
                   , _thinLensExposure :: Double
                   , _thinLensZ :: Double
                   , _thinLensVpDist :: Double
                   , _thinLensFpDist :: Double
                   , _thinLensRadius :: AnimDouble
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
instance Serialize AnimDouble where

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
