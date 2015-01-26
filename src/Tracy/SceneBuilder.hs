module Tracy.SceneBuilder
  ( sceneFromDesc
  )
  where

import Control.Applicative
import Control.Lens ((^.))
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Either
import Data.Traversable (sequenceA)
import Data.Monoid
import Linear (V3)

import Tracy.Types
import Tracy.Cameras

import Tracy.Materials.Emissive
import Tracy.Materials.Phong
import Tracy.Materials.Matte

import Tracy.Objects.Sphere
import Tracy.Objects.Box
import Tracy.Objects.Grid
import Tracy.Objects.Plane
import Tracy.Objects.Rectangle
import Tracy.Objects.Triangle
import Tracy.Objects.Mesh
import Tracy.Objects.Instance

import Tracy.Lights.Ambient
import Tracy.Lights.AmbientOccluder
import Tracy.Lights.Area
import Tracy.Lights.Environment
import Tracy.Lights.Point

import Tracy.Anim ()
import Tracy.Tracers
import Tracy.AccelSchemes
import Tracy.Samplers
import Tracy.Transformations

type LoadM a = EitherT String IO a

sceneFromDesc :: SceneDesc -> Int -> IO (Either String (Scene ThinLens))
sceneFromDesc sd fn = runEitherT (runSceneFromDesc sd fn)

runSceneFromDesc :: SceneDesc -> Int -> LoadM (Scene ThinLens)
runSceneFromDesc sd fn =
    Scene <$> (worldFromDesc fn    $ sd^.sceneDescWorld)
          <*> (accelSchemeFromDesc $ sd^.sceneDescAccelScheme)
          <*> (cameraFromDesc fn   $ sd^.sceneDescCamera)
          <*> (tracerFromDesc      $ sd^.sceneDescTracer)

worldFromDesc :: Int -> WorldDesc -> LoadM World
worldFromDesc fn wd =
    World <$> (pure               $ wd^.wdViewPlane)
          <*> (pure               $ wd^.wdBgColor)
          <*> (objectsFromDesc fn $ wd^.wdObjects)
          <*> (lightsFromDesc fn  $ wd^.wdLights)
          <*> (lightFromDesc fn   $ wd^.wdAmbient)
          <*> (pure               $ wd^.wdWorldShadows)

objectsFromDesc :: Int -> [ObjectDesc] -> LoadM [Object]
objectsFromDesc fn os = concat <$> sequenceA (objectFromDesc fn <$> os)

single :: LoadM b -> LoadM [b]
single v = (:[]) <$> v

objectFromDesc :: Int -> ObjectDesc -> LoadM [Object]
objectFromDesc fn (Sphere c r m) = single $ sphere c r <$> materialFromDesc fn m
objectFromDesc fn (ConcaveSphere c r m) = single $ concaveSphere c r <$> materialFromDesc fn m
objectFromDesc fn (Rectangle p0 a b m) = single $ rectangle p0 a b <$> materialFromDesc fn m
objectFromDesc fn (Triangle v1 v2 v3 m) = single $ tri v1 v2 v3 <$> materialFromDesc fn m
objectFromDesc fn (Box v1 v2 m) = single $ box v1 v2 <$> materialFromDesc fn m
objectFromDesc fn (Plane c norm m) = single $ plane c norm <$> materialFromDesc fn m
objectFromDesc fn (Mesh (MeshFile path) m) = do
    mData <- liftIO $ loadMesh path
    theMesh <- mesh mData <$> materialFromDesc fn m
    return [theMesh]
objectFromDesc fn (Grid os) = single $ grid <$> (concat <$> sequenceA (objectFromDesc fn <$> os))
objectFromDesc fn (Instances oDesc is) = do
    v <- objectFromDesc fn oDesc
    ids <- sequenceA $ instanceDataFromDesc fn <$> is
    case v of
        [o] -> return $ (\(t, m) -> inst t m o) <$> ids
        _ -> error "Error: Instances of (multiple) Instances not allowed"

instanceDataFromDesc :: Int -> InstanceDesc -> LoadM (Transformation, Maybe Material)
instanceDataFromDesc fn (ID tDesc mDesc) = do
    mResult <- case mDesc of
                   Nothing -> return Nothing
                   Just md -> Just <$> materialFromDesc fn md
    ts <- sequenceA (transformationFromDesc <$> tDesc)
    return (mconcat ts, mResult)

transformationFromDesc :: TransformationDesc -> LoadM Transformation
transformationFromDesc (Translate x y z) = return $ translate x y z
transformationFromDesc (Scale x y z) = return $ scale x y z
transformationFromDesc (ScaleUni f) = return $ scaleUni f
transformationFromDesc (RotateX v) = return $ rotateX v
transformationFromDesc (RotateY v) = return $ rotateY v
transformationFromDesc (RotateZ v) = return $ rotateZ v

lightsFromDesc :: Int -> [LightDesc] -> LoadM [Light]
lightsFromDesc fn ls = sequenceA (lightFromDesc fn <$> ls)

lightFromDesc :: Int -> LightDesc -> LoadM Light
lightFromDesc _ (Ambient s c) = return $ ambientLight s c
lightFromDesc _ (AmbientOccluder c min_amt s) = return $ ambientOccluder c min_amt s
lightFromDesc _ (Point sh ls c loc) = return $ pointLight sh ls c loc
lightFromDesc fn (Environment sh m) = environmentLight sh <$> (materialFromDesc fn m)
lightFromDesc fn (Area sh oDesc p) = do
    v <- objectFromDesc fn oDesc
    case v of
      [o] -> return $ areaLight sh o p
      _ -> fail "Could not create area light from multiple objects"

accelSchemeFromDesc :: AccelSchemeDesc -> LoadM AccelScheme
accelSchemeFromDesc NoScheme = return noScheme
accelSchemeFromDesc GridScheme = return gridScheme

materialFromDesc :: Int -> MaterialDesc -> LoadM Material
materialFromDesc _ (Matte c) = return $ matteFromColor c
materialFromDesc _ (Phong c ks e) = return $ phongFromColor c ks e
materialFromDesc _ (Reflective c ks e cr kr) = return $ reflective c ks e cr kr
materialFromDesc _ (GlossyReflective c ks e cr kr er) = return $ glossyReflective c ks e cr kr er
materialFromDesc _ (Emissive c e) = return $ emissive c e

tracerFromDesc :: TracerDesc -> LoadM Tracer
tracerFromDesc RayCastTracer = return rayCastTracer
tracerFromDesc PathTracer = return pathTracer
tracerFromDesc AreaLightTracer = return areaLightTracer

v2SamplerFromDesc :: V2SamplerDesc -> LoadM (Sampler (Double, Double))
v2SamplerFromDesc Regular = return regular
v2SamplerFromDesc PureRandom = return pureRandom
v2SamplerFromDesc Jittered = return jittered
v2SamplerFromDesc MultiJittered = return multiJittered
v2SamplerFromDesc CorrelatedMultiJittered = return correlatedMultiJittered
v2SamplerFromDesc (UnitDisk sd) = (toUnitDisk <$>) <$> v2SamplerFromDesc sd

v3SamplerFromDesc :: V3SamplerDesc -> LoadM (Sampler (V3 Double))
v3SamplerFromDesc (UnitHemi e sd) = (toUnitHemi e <$>) <$> v2SamplerFromDesc sd

cameraFromDesc :: Int -> CameraDesc -> LoadM (Camera ThinLens)
cameraFromDesc fn cd@(ThinLensCamera { }) =
    thinLensCamera (animate fn $ cd^.thinLensEye)
                   (cd^.thinLensLookAt)
                   (cd^.thinLensUp)
                   (cd^.thinLensExposure)
                   (cd^.thinLensZ)
                   (cd^.thinLensVpDist)
                   (cd^.thinLensFpDist)
                   (animate fn $ cd^.thinLensRadius)
                   <$> (v2SamplerFromDesc $ cd^.thinLensSampler)
