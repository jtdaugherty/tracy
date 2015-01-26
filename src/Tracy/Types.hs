{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances, DeriveGeneric, DefaultSignatures, BangPatterns, MultiParamTypeClasses, OverloadedStrings #-}
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
import qualified Data.Yaml as Y
import qualified Data.Text as T

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
data Transformation = Trans !(M44 Float, M44 Float)
    deriving (Eq, Read, Show, Generic)

instance Monoid Transformation where
    mempty = Trans (eye4, eye4)
    (Trans (f1, i1)) `mappend` (Trans (f2, i2)) =
        Trans (f1 !*! f2, i2 !*! i1)

data Shade =
    Shade { _localHitPoint :: !(V3 Float)
          , _normal :: !(V3 Float)
          , _material :: Material
          , _shadeRay :: !Ray
          , _depth :: !Int
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
           , _bounding_box :: !(Maybe BBox)
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
                     deriving (Show, Eq, Generic, Read)

data V3SamplerDesc = UnitHemi Float V2SamplerDesc
                     deriving (Show, Eq, Generic, Read)

data TraceData =
    TD { _tdHemiSample :: !(V3 Float)
       , _tdHemiSampleExp :: Float -> V3 Float
       , _tdDiskSample :: !(V2 Float)
       , _tdSquareSample :: !(V2 Float)
       , _tdObjectSurfaceSample :: !(V2 Float)
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
    deriving (Eq, Show, Generic, Read)

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
    deriving (Eq, Show, Generic, Read)

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

instance Y.FromJSON ViewPlane where
    parseJSON (Y.Object v) =
        ViewPlane <$> v Y..: "hres"
                  <*> v Y..: "vres"
                  <*> v Y..: "pixelSize"
                  <*> v Y..: "gamma"
                  <*> v Y..: "inverseGamma"
                  <*> v Y..: "maxDepth"
    parseJSON _ = fail "Expected object for ViewPlane"

readsT :: (Read a) => T.Text -> [(a, String)]
readsT t = reads (T.unpack t)

parseReadsT :: (Read a) => T.Text -> String -> Y.Parser a
parseReadsT t errMsg =
    case readsT t of
        [(v, "")] -> return v
        _ -> fail errMsg

instance Y.FromJSON TracerDesc where
    parseJSON (Y.String s) = parseReadsT s "Invalid TracerDesc value"
    parseJSON _ = fail "Expected string for TracerDesc"

instance Y.FromJSON AccelSchemeDesc where
    parseJSON (Y.String s) = parseReadsT s "Invalid AccelSchemeDesc value"
    parseJSON _ = fail "Expected string for AccelSchemeDesc"

instance Y.FromJSON (V3 Float) where
    parseJSON (Y.String s) = parseReadsT s "Invalid V3 value"
    parseJSON _ = fail "Expected string for V3"

instance Y.FromJSON AnimV3 where
    parseJSON (Y.Object v) =
        V3Val <$> v Y..: "const"
        -- V3Lerp (Int, Int) (V3 Float, V3 Float)
        -- V3LerpRotY (Int, Int) (V3 Float) Float
    parseJSON _ = fail "Expected object for AnimV3"

instance Y.FromJSON AnimFloat where
    parseJSON (Y.Object v) =
        FloatVal <$> v Y..: "const"
        -- FloatLerp (Int, Int) (Float, Float)
    parseJSON _ = fail "Expected object for AnimV3"

instance Y.FromJSON CameraDesc where
    parseJSON (Y.Object v) =
        ThinLensCamera <$> v Y..: "eye"
                       <*> v Y..: "lookAt"
                       <*> v Y..: "up"
                       <*> v Y..: "exposure"
                       <*> v Y..: "zoom"
                       <*> v Y..: "vpDist"
                       <*> v Y..: "fpDist"
                       <*> v Y..: "lensRadius"
                       <*> v Y..: "lensSampler"
    parseJSON _ = fail "Expected object for CameraDesc"

instance Y.FromJSON V2SamplerDesc where
    parseJSON (Y.String s) = parseReadsT s "Invalid V2SamplerDesc value"
    parseJSON _ = fail "Expected string for V2SamplerDesc"

instance Y.FromJSON V3SamplerDesc where
    parseJSON (Y.String s) = parseReadsT s "Invalid V3SamplerDesc value"
    parseJSON _ = fail "Expected string for V3SamplerDesc"

instance Y.FromJSON Color where
    parseJSON (Y.String _) = pure cBlack
    parseJSON _ = fail "Expected string for Color value"

instance Y.FromJSON MaterialDesc where
    parseJSON (Y.Object v) = do
        t <- v Y..: "type"
        case t of
            "phong" -> Phong <$> v Y..: "color"
                             <*> v Y..: "ks"
                             <*> v Y..: "exp"
            t' -> fail $ "Unsupported material type: " ++ (show $ T.unpack t')
        -- Matte Color
        -- Phong Color Float Float
        -- Emissive Color Float
        -- Reflective Color Float Float Color Float
        -- GlossyReflective Color Float Float Color Float Float
    parseJSON _ = fail "Expected object for MaterialDesc"

instance Y.FromJSON LightDesc where
    parseJSON (Y.Object v) = do
        t <- v Y..: "type"
        case t of
            "point" -> Point <$> v Y..: "shadows"
                             <*> v Y..: "strength"
                             <*> v Y..: "color"
                             <*> v Y..: "position"
            "ambient" -> Ambient <$> v Y..: "strength"
                                 <*> v Y..: "color"
            t' -> fail $ "Unsupported material type: " ++ (show $ T.unpack t')

        -- AmbientOccluder Color Color Float
        -- Area Bool ObjectDesc (Maybe Float)
        -- Environment Bool MaterialDesc
    parseJSON _ = fail "Expected object for LightDesc"

instance Y.FromJSON ObjectDesc where
    parseJSON (Y.Object v) = do
        t <- v Y..: "type"
        case t of
            "sphere" -> Sphere <$> v Y..: "center"
                               <*> v Y..: "radius"
                               <*> v Y..: "material"
            t' -> fail $ "Unsupported object type: " ++ (show $ T.unpack t')
    -- ConcaveSphere (V3 Float) Float MaterialDesc
    -- Rectangle (V3 Float) (V3 Float) (V3 Float) MaterialDesc
    -- Triangle (V3 Float) (V3 Float) (V3 Float) MaterialDesc
    -- Box (V3 Float) (V3 Float) MaterialDesc
    -- Plane (V3 Float) (V3 Float) MaterialDesc
    -- Mesh MeshDesc MaterialDesc
    -- Instances ObjectDesc [(Transformation, Maybe MaterialDesc)]
    -- Grid [ObjectDesc]
    parseJSON _ = fail "Expected object for ObjectDesc"

instance Y.FromJSON WorldDesc where
    parseJSON (Y.Object v) =
        WorldDesc <$> v Y..: "viewPlane"
                  <*> v Y..: "bgColor"
                  <*> v Y..: "objects"
                  <*> v Y..: "lights"
                  <*> v Y..: "ambient"
                  <*> v Y..: "shadows"
    parseJSON _ = fail "Expected object for WorldDesc"

instance Y.FromJSON SceneDesc where
    parseJSON (Y.Object v) =
        SceneDesc <$> v Y..: "world"
                  <*> v Y..: "accel"
                  <*> v Y..: "camera"
                  <*> v Y..: "tracer"
    parseJSON _ = fail "Expected object for SceneDesc"

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
