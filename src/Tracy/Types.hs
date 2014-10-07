{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances, DeriveGeneric, DefaultSignatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tracy.Types where

import Control.Applicative
import Control.Lens
import Data.Serialize
import Data.Time.Clock
import Control.Monad.Reader
import qualified Data.Vector as V
import GHC.Generics
import Linear
import Data.Colour
import Data.Monoid

type Color = Colour

data InfoEvent =
      ISampleRoot Float
    | IConnected String
    | IConnecting String
    | ISceneName String
    | IAccelScheme AccelSchemeDesc
    | INumObjects Int
    | IShadows Bool
    | INumCPUs Int
    | INumChunks Int
    | INumRowsPerChunk Int
    | IChunkFinished Int Int NominalDiffTime
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
    | DNumChunks Int
    | DChunkFinished Int (Int, Int) [[Colour]]
    | DImageSize Int Int
    | DStarted
    | DFinished
    | DShutdown
    deriving (Eq, Show)

data JobRequest =
      SetScene RenderConfig SceneDesc
    | RenderRequest Int (Int, Int)
    | RenderFinished
    | Shutdown
    deriving (Generic, Show)

data JobResponse =
      JobError String
    | ChunkFinished Int (Int, Int) [[Colour8]]
    | JobAck
    deriving (Generic)

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

data ObjectAreaLightImpl =
    ObjectALI { _objectSurfaceSample :: TraceM (V3 Float)
              , _objectGetNormal :: V3 Float -> V3 Float
              , _objectPDF :: Shade -> Float
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
          , _lightPDF :: Shade -> Float
          }

data Material =
    Material { _doShading :: Shade -> TraceM Color
             , _doAreaShading :: Shade -> TraceM Color
             , _getLe :: Shade -> Color
             }

data BBox =
    BBox { _bboxP0 :: V3 Float
         , _bboxP1 :: V3 Float
         }
         deriving (Show)

type Sampler a = Float -> IO [a]

type CameraRenderer a = Camera a
                      -> Int
                      -> RenderConfig
                      -> World
                      -> Tracer
                      -> V.Vector [(Float, Float)]
                      -> V.Vector [(Float, Float)]
                      -> (Int, [Int])
                      -> [Color]

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

data TraceData =
    TD { _tdHemiSample :: V3 Float
       , _tdDiskSample :: V2 Float
       , _tdSquareSample :: V2 Float
       , _tdWorld :: World
       , _tdWorldHitFuncs :: [Ray -> Maybe (Shade, Float)]
       , _tdWorldShadowHitFuncs :: [Ray -> Maybe Float]
       }

type TraceM a = Reader TraceData a

data Tracer =
    Tracer { _doTrace :: Ray -> TraceM Color
           }

---------------------------------------------------------------------------
-- World description types for transmission between rendering nodes
---------------------------------------------------------------------------
data TracerDesc =
    RayCastTracer
  | AreaLightTracer
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
             , meshDescFaces :: [[Int]]
             }
    deriving (Eq, Show)

data ObjectDesc =
      Sphere (V3 Float) Float MaterialDesc
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
    | Area Bool ObjectDesc
    deriving (Eq, Show, Generic)

data MaterialDesc =
      Matte Color
    | Phong Color Float
    | Emissive Color Float
    deriving (Eq, Show, Generic)

data CameraDesc =
    ThinLensCamera { _thinLensEye :: V3 Float
                   , _thinLensLookAt :: V3 Float
                   , _thinLensUp :: V3 Float
                   , _thinLensExposure :: Float
                   , _thinLensZ :: Float
                   , _thinLensVpDist :: Float
                   , _thinLensFpDist :: Float
                   , _thinLensRadius :: Float
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

instance Serialize SceneDesc where
instance Serialize WorldDesc where
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

instance Serialize MeshDesc where
    get = MeshDesc <$> (V.fromList <$> get) <*> get
    put (MeshDesc vs fs) = put (V.toList vs) >> put fs

makeLenses ''Shade
makeLenses ''Ray
makeLenses ''Object
makeLenses ''ViewPlane
makeLenses ''World
makeLenses ''BRDF
makeLenses ''BRDFData
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

makeLenses ''SceneDesc
makeLenses ''WorldDesc
makeLenses ''CameraDesc
