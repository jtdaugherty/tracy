{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances, DeriveGeneric, DefaultSignatures, BangPatterns, MultiParamTypeClasses, OverloadedStrings, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tracy.Types where

import Control.Applicative
import Control.Lens
import Data.Serialize
import Data.Time.Clock
import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Vector.Storable as SV
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import GHC.Generics
import Linear
import Data.Colour
import Data.Monoid
import Data.Word
import System.Random.MWC
import Foreign.Storable
import Foreign.C.Types
import Foreign.Ptr
import Data.Vector.Cereal ()
import qualified Data.Yaml as Y
import qualified Data.Text as T

type Color = Colour

newtype Frame = Frame Int
              deriving (Eq, Show, Generic)

newtype Row = Row Int
              deriving (Eq, Show, Generic, Enum, Ord)

-- A depth value.
newtype Depth = Depth Int
              deriving (Eq, Show, Generic, Num, Ord)

-- The size of some set of things or the value of a counter.
newtype Count = Count Int
              deriving (Eq, Show, Generic)

newtype Width = Width Int
              deriving (Eq, Show, Generic)

newtype Height = Height Int
              deriving (Eq, Show, Generic)

data InfoEvent =
      ISampleRoot Double
    | IFrameNum Frame
    | IConnected String
    | IConnecting String
    | INodeReady String
    | ISceneName String
    | IAccelScheme AccelSchemeDesc
    | INumObjects Count
    | IShadows Bool
    | INumCPUs Count
    | IChunkFinished Count Count NominalDiffTime
    | IStartTime UTCTime
    | IFinishTime UTCTime
    | ITotalTime NominalDiffTime
    | IImageSize Width Height
    | ILoadedMeshes Count
    | ILoadingMeshes
    | ISettingScene
    | IStarted
    | IFinished
    | IShutdown
    deriving (Eq)

data DataEvent =
      DSceneName String
    | DFrameNum Frame
    | DSampleRoot Double
    | DChunkFinished (Row, Row) (SV.Vector Colour)
    | DImageSize Width Height
    | DRowRanges [(Row, Row)]
    | DStarted
    | DFinished
    | DShutdown
    deriving (Eq, Show)

data JobRequest =
      SetScene RenderConfig SceneDesc MeshGroup Frame (VU.Vector Word32) [(Row, Row)]
    | RenderRequest (Row, Row) (Int, Int)
    | RenderFinished
    | Shutdown
    deriving (Generic, Show)

type MeshGroup = Map MeshSource MeshData

data JobResponse =
      JobError String
    | ChunkFinished (Row, Row) (SV.Vector Colour)
    | SetSceneAck
    | JobAck
    deriving (Generic)

class Anim a b where
    animate :: Frame -> a -> b

data AnimV3 =
      V3Val (V3 Double)
    | V3Lerp (Frame, Frame) (V3 Double, V3 Double)
    | V3LerpRotY (Frame, Frame) (V3 Double) Double
    deriving (Generic, Eq, Show)

data AnimDouble =
      DoubleVal Double
    | DoubleLerp (Frame, Frame) (Double, Double)
    deriving (Generic, Eq, Show)

-- A transformation is a pair of (forward transformation matrix, inverse
-- transformation matrix).  Note the Monoid instance for this type.
data Transformation = Trans !(M44 Double, M44 Double)
    deriving (Eq, Read, Show, Generic)

data TransformationDesc =
    Translate Double Double Double
    | Scale Double Double Double
    | ScaleUni Double
    | RotateX Double
    | RotateY Double
    | RotateZ Double
    deriving (Eq, Show, Read, Generic)

instance Monoid Transformation where
    mempty = Trans (eye4, eye4)
    (Trans (f1, i1)) `mappend` (Trans (f2, i2)) =
        Trans (f1 !*! f2, i2 !*! i1)

data Shade =
    Shade { _localHitPoint :: !(V3 Double)
          , _normal :: !(V3 Double)
          , _material :: Material
          , _shadeRay :: !Ray
          , _depth :: !Depth
          }

data Ray =
    Ray { _origin :: !(V3 Double)
        , _direction :: !(V3 Double)
        }
    deriving (Show)

data ObjectAreaLightImpl =
    ObjectALI { _objectSurfaceSample :: TraceM (V3 Double)
              , _objectGetNormal :: Shade -> V3 Double -> V3 Double
              , _objectPDF :: LightDir -> Shade -> Double
              }

data Object =
    Object { _objectMaterial :: Material
           , _hit :: Ray -> Maybe (Shade, Double)
           , _shadow_hit :: Ray -> Maybe Double
           , _bounding_box :: !(Maybe BBox)
           , _areaLightImpl :: Maybe ObjectAreaLightImpl
           }

data ViewPlane =
    ViewPlane { _hres :: Double
              , _vres :: Double
              , _pixelSize :: Double
              , _gamma :: Double
              , _inverseGamma :: Double
              , _maxDepth :: Depth
              , _pixelSampler :: Sampler (Double, Double)
              }
    deriving (Generic)

data ViewPlaneDesc =
    ViewPlaneDesc { _vpHres :: Double
                  , _vpVres :: Double
                  , _vpPixelSize :: Double
                  , _vpGamma :: Double
                  , _vpInverseGamma :: Double
                  , _vpMaxDepth :: Depth
                  , _vpPixelSampler :: V2SamplerDesc
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

data RenderMode =
    BreadthFirst
    | DepthFirst
    deriving (Eq, Generic, Show)

data RenderConfig =
    RenderConfig { _sampleRoot :: Double
                 , _forceShadows :: Maybe Bool
                 , _samplesPerChunk :: Int
                 , _rowsPerChunk :: Height
                 , _renderMode :: RenderMode
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
               , _pixelSampleSets :: !(V.Vector (V.Vector (Double, Double)))
               , _squareSampleSets :: !(V.Vector (V.Vector (Double, Double)))
               , _diskSampleSets :: !(V.Vector (V.Vector (Double, Double)))
               , _objectSampleSets :: !(V.Vector (V.Vector (Double, Double)))
               }

type CameraRenderer a = Camera a
                      -> RenderConfig
                      -> World
                      -> Tracer
                      -> SampleData
                      -> (Row, V.Vector Int)
                      -> (Int, Int)
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
                     deriving (Show, Eq, Generic, Read)

data V3SamplerDesc = UnitHemi Double V2SamplerDesc
                     deriving (Show, Eq, Generic, Read)

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
    Tracer { _doTrace :: Ray -> Depth -> TraceM Color
           }

---------------------------------------------------------------------------
-- World description types for transmission between rendering nodes
---------------------------------------------------------------------------
data TracerDesc =
    RayCastTracer
  | AreaLightTracer
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
    WorldDesc { _wdViewPlane :: ViewPlaneDesc
              , _wdBgColor :: Color
              , _wdObjects :: [ObjectDesc]
              , _wdLights :: [LightDesc]
              , _wdAmbient :: LightDesc
              , _wdWorldShadows :: Bool
              }
    deriving (Eq, Show, Generic)

data MeshData =
    MeshData { meshVertices :: V.Vector (V3 Double, V3 Double)
             , meshFaces :: [V.Vector Int]
             }
    deriving (Eq, Show)

data ObjectDesc =
      Sphere (V3 Double) Double MaterialDesc
    | ConcaveSphere (V3 Double) Double MaterialDesc
    | Rectangle (V3 Double) (V3 Double) (V3 Double) Bool MaterialDesc
    | Triangle (V3 Double) (V3 Double) (V3 Double) MaterialDesc
    | Box (V3 Double) (V3 Double) MaterialDesc
    | Plane (V3 Double) (V3 Double) MaterialDesc
    | Mesh MeshSource MaterialDesc
    | Instances ObjectDesc [InstanceDesc]
    | Grid [ObjectDesc]
    deriving (Eq, Show, Generic)

data InstanceDesc = ID [TransformationDesc] (Maybe MaterialDesc)
                  deriving (Eq, Show, Generic)

data MeshSource =
    MeshFile FilePath
    deriving (Eq, Show, Generic, Ord)

data LightDesc =
      Ambient Double Color
    | AmbientOccluder Color Color Double
    | Point Bool Double Color (V3 Double)
    | Area Bool ObjectDesc (Maybe Double)
    | Environment Bool MaterialDesc
    deriving (Eq, Show, Generic)

data MaterialDesc =
      Matte Color
    | Mix AnimDouble MaterialDesc MaterialDesc
    | Add MaterialDesc MaterialDesc
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
                   , _thinLensVpDist :: AnimDouble
                   , _thinLensFpDist :: AnimDouble
                   , _thinLensRadius :: AnimDouble
                   , _thinLensSampler :: V2SamplerDesc
                   }
    deriving (Eq, Show, Generic)

class HasMeshes a where
    findMeshes :: a -> [MeshSource]

instance HasMeshes a => HasMeshes [a] where
    findMeshes = concat . (findMeshes <$>)

instance HasMeshes ObjectDesc where
    findMeshes (Mesh s _) = [s]
    findMeshes (Instances o _) = findMeshes o
    findMeshes (Grid os) = concat $ findMeshes <$> os
    findMeshes _ = []

instance HasMeshes LightDesc where
    findMeshes (Area _ o _) = findMeshes o
    findMeshes _ = []

instance HasMeshes WorldDesc where
    findMeshes w = concat [ findMeshes (_wdObjects w)
                          , findMeshes (_wdAmbient w)
                          , findMeshes (_wdLights w)
                          ]

instance HasMeshes SceneDesc where
    findMeshes sd = findMeshes $ _sceneDescWorld sd

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

instance Serialize RenderMode where
instance Serialize Frame where
instance Serialize Row where
instance Serialize Count where
instance Serialize Height where
instance Serialize Width where
instance Serialize Depth where
instance Serialize SceneDesc where
instance Serialize WorldDesc where
instance Serialize V2SamplerDesc where
instance Serialize V3SamplerDesc where
instance Serialize CameraDesc where
instance Serialize ObjectDesc where
instance Serialize LightDesc where
instance Serialize MaterialDesc where
instance Serialize AccelSchemeDesc where
instance Serialize TransformationDesc where
instance Serialize InstanceDesc where
instance Serialize TracerDesc where
instance Serialize ViewPlaneDesc where
instance Serialize JobRequest where
instance Serialize JobResponse where
instance Serialize RenderConfig where
instance Serialize Transformation where
instance Serialize AnimV3 where
instance Serialize AnimDouble where
instance Serialize MeshSource where

instance Serialize MeshData where
    get = MeshData <$> (V.fromList <$> get) <*> get
    put (MeshData vs fs) = put (V.toList vs) >> put fs

instance Y.FromJSON ViewPlaneDesc where
    parseJSON (Y.Object v) =
        ViewPlaneDesc <$> v Y..: "hres"
                      <*> v Y..: "vres"
                      <*> v Y..: "pixelSize"
                      <*> v Y..: "gamma"
                      <*> v Y..: "inverseGamma"
                      <*> (Depth <$> v Y..: "maxDepth")
                      <*> v Y..: "pixelSampler"
    parseJSON _ = fail "Expected object for ViewPlane"

readsT :: (Read a) => T.Text -> [(a, String)]
readsT t = reads (T.unpack t)

parseReadsT :: (Read a) => T.Text -> String -> Y.Parser a
parseReadsT t errMsg =
    case readsT t of
        [(v, "")] -> return v
        _ -> fail errMsg

instance Read Colour where
    readsPrec d r = readParen (d > app_prec) action r
      where
        app_prec = 10
        action next = do
            ("Colour", n1) <- lex next
            (d1, n2) <- readsPrec (app_prec+1) n1
            (d2, n3) <- readsPrec (app_prec+1) n2
            (d3, n4) <- readsPrec (app_prec+1) n3
            return (Colour d1 d2 d3, n4)

instance Y.FromJSON TracerDesc where
    parseJSON (Y.String s) = parseReadsT s "Invalid TracerDesc value"
    parseJSON _ = fail "Expected string for TracerDesc"

instance Y.FromJSON AccelSchemeDesc where
    parseJSON (Y.String s) = parseReadsT s "Invalid AccelSchemeDesc value"
    parseJSON _ = fail "Expected string for AccelSchemeDesc"

instance Y.FromJSON (V3 Double) where
    parseJSON (Y.String s) = parseReadsT s "Invalid V3 value"
    parseJSON _ = fail "Expected string for V3"

instance Y.FromJSON AnimV3 where
    parseJSON (Y.Object v) = do
        t <- v Y..: "type"
        case (t::T.Text) of
            "const" -> V3Val <$> v Y..: "value"
            "lerp" -> V3Lerp <$> ((,) <$> (Frame <$> (v Y..: "fromFrame"))
                                      <*> (Frame <$> (v Y..: "toFrame"))
                                 )
                             <*> ((,) <$> (v Y..: "from")
                                      <*> (v Y..: "to")
                                 )
            "lerpRotY" -> V3LerpRotY <$> ((,) <$> (Frame <$> (v Y..: "fromFrame"))
                                              <*> (Frame <$> (v Y..: "toFrame"))
                                         )
                                     <*> (v Y..: "from")
                                     <*> (v Y..: "angle")
            t' -> fail $ "Invalid AnimV3 type: " ++ (show $ T.unpack t')
    parseJSON _ = fail "Expected object for AnimV3"

instance Y.FromJSON AnimDouble where
    parseJSON (Y.Object v) = do
        t <- v Y..: "type"
        case (t::T.Text) of
            "const" -> DoubleVal <$> v Y..: "value"
            "lerp" -> DoubleLerp <$> ((,) <$> (Frame <$> (v Y..: "fromFrame"))
                                          <*> (Frame <$> (v Y..: "toFrame"))
                                     )
                                 <*> ((,) <$> (v Y..: "from")
                                          <*> (v Y..: "to")
                                     )
            t' -> fail $ "Invalid AnimDouble type: " ++ (show $ T.unpack t')
    parseJSON _ = fail "Expected object for AnimDouble"

instance Y.FromJSON CameraDesc where
    parseJSON (Y.Object v) =
        ThinLensCamera <$> ((V3Val <$> v Y..: "eye") <|> (v Y..: "eye"))
                       <*> v Y..: "lookAt"
                       <*> v Y..: "up"
                       <*> v Y..: "exposure"
                       <*> v Y..: "zoom"
                       <*> ((DoubleVal <$> v Y..: "vpDist") <|> (v Y..: "vpDist"))
                       <*> ((DoubleVal <$> v Y..: "fpDist") <|> (v Y..: "fpDist"))
                       <*> ((DoubleVal <$> v Y..: "lensRadius") <|> (v Y..: "lensRadius"))
                       <*> v Y..: "lensSampler"
    parseJSON _ = fail "Expected object for CameraDesc"

instance Y.FromJSON V2SamplerDesc where
    parseJSON (Y.String s) = parseReadsT s "Invalid V2SamplerDesc value"
    parseJSON _ = fail "Expected string for V2SamplerDesc"

instance Y.FromJSON V3SamplerDesc where
    parseJSON (Y.String s) = parseReadsT s "Invalid V3SamplerDesc value"
    parseJSON _ = fail "Expected string for V3SamplerDesc"

instance Y.FromJSON Color where
    parseJSON (Y.String s) = parseReadsT s "Invalid Colour value"
    parseJSON _ = fail "Expected string for Color value"

instance Y.FromJSON MaterialDesc where
    parseJSON (Y.Object v) = do
        t <- v Y..: "type"
        case t of
            "phong" -> Phong <$> v Y..: "color"
                             <*> v Y..: "ks"
                             <*> v Y..: "exp"
            "matte" -> Matte <$> v Y..: "color"
            "add" -> Add <$> v Y..: "first"
                         <*> v Y..: "second"
            "mix" -> Mix <$> v Y..: "amount"
                         <*> v Y..: "first"
                         <*> v Y..: "second"
            "emissive" -> Emissive <$> v Y..: "color"
                                   <*> v Y..: "strength"
            "reflective" -> Reflective <$> v Y..: "baseColor"
                                       <*> v Y..: "ks"
                                       <*> v Y..: "exp"
                                       <*> v Y..: "reflectiveColor"
                                       <*> v Y..: "reflectiveStrength"
            "glossyReflective" -> GlossyReflective <$> v Y..: "baseColor"
                                                   <*> v Y..: "ks"
                                                   <*> v Y..: "exp"
                                                   <*> v Y..: "reflectiveColor"
                                                   <*> v Y..: "kr"
                                                   <*> v Y..: "expR"
            t' -> fail $ "Unsupported material type: " ++ (show $ T.unpack t')
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
            "ambientOccluder" -> AmbientOccluder <$> v Y..: "color"
                                                 <*> v Y..: "minAmount"
                                                 <*> v Y..: "strength"
            "area" -> Area <$> v Y..: "shadows"
                           <*> v Y..: "object"
                           <*> v Y..:? "invArea"
            t' -> fail $ "Unsupported material type: " ++ (show $ T.unpack t')

        -- Environment Bool MaterialDesc
    parseJSON _ = fail "Expected object for LightDesc"

instance Y.FromJSON TransformationDesc where
    parseJSON (Y.String s) = parseReadsT s "Invalid transformation value"
    parseJSON _ = fail "Expected string for Transformation"

instance Y.FromJSON ObjectDesc where
    parseJSON (Y.Object v) = parseInstances <|> parseGrid <|> parseObject
        where
          parseInstances = Instances <$> v Y..: "master"
                                     <*> v Y..: "objects"
          parseGrid = Grid <$> v Y..: "objects"
          parseObject = do
            mat <- v Y..: "material"
            t <- v Y..: "type"
            obj <- case t of
                "sphere" -> Sphere <$> v Y..: "center"
                                   <*> v Y..: "radius"
                                   <*> (pure mat)
                "plane" -> Plane <$> v Y..: "origin"
                                 <*> v Y..: "normal"
                                 <*> (pure mat)
                "box" -> Box <$> v Y..: "from"
                             <*> v Y..: "to"
                             <*> (pure mat)
                "tri" -> Triangle <$> v Y..: "v1"
                                  <*> v Y..: "v2"
                                  <*> v Y..: "v3"
                                  <*> (pure mat)
                "mesh" -> Mesh <$> v Y..: "path"
                               <*> (pure mat)
                "rect" -> Rectangle <$> v Y..: "corner"
                                    <*> v Y..: "over"
                                    <*> v Y..: "up"
                                    <*> (v Y..: "doubleSided" <|> pure True)
                                    <*> (pure mat)
                "concaveSphere" -> ConcaveSphere <$> v Y..: "center"
                                                 <*> v Y..: "radius"
                                                 <*> (pure mat)
                t' -> fail $ "Unsupported object type: " ++ (show $ T.unpack t')

            (trans::Maybe [TransformationDesc]) <- v Y..:? "transform"
            case trans of
                Nothing -> return obj
                Just tds -> return $ Instances obj [ID tds Nothing]

    parseJSON _ = fail "Expected object for ObjectDesc"

instance Y.FromJSON MeshSource where
    parseJSON (Y.String s) = return $ MeshFile $ T.unpack s
    parseJSON _ = fail "Expected string for MeshSource"

instance Y.FromJSON InstanceDesc where
    parseJSON (Y.Object v) =
        ID <$> v Y..: "transform"
           <*> v Y..:? "material"
    parseJSON _ = fail "Expected object for InstanceDesc"

instance Y.FromJSON WorldDesc where
    parseJSON (Y.Object v) =
        WorldDesc <$> v Y..: "viewPlane"
                  <*> v Y..: "bgColor"
                  <*> v Y..: "objects"
                  <*> (v Y..: "lights" <|> pure [])
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
makeLenses ''ViewPlaneDesc
makeLenses ''SceneDesc
makeLenses ''WorldDesc
makeLenses ''CameraDesc
