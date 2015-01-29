module Tracy.SceneBuilder
  ( sceneFromDesc
  )
  where

import Control.Applicative
import Control.Lens ((^.))
import Data.Traversable (sequenceA)
import Data.Monoid
import qualified Data.Map as M
import Linear (V3)

import Tracy.Types
import Tracy.Cameras

import Tracy.Materials.Emissive
import Tracy.Materials.Phong
import Tracy.Materials.Matte
import Tracy.Materials.Mix

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

type LoadM a = Either String a

sceneFromDesc :: SceneDesc -> MeshGroup -> Int -> Either String (Scene ThinLens)
sceneFromDesc sd mg fn = runSceneFromDesc sd mg fn

runSceneFromDesc :: SceneDesc -> MeshGroup -> Int -> LoadM (Scene ThinLens)
runSceneFromDesc sd mg fn =
    Scene <$> (worldFromDesc mg fn    $ sd^.sceneDescWorld)
          <*> (accelSchemeFromDesc    $ sd^.sceneDescAccelScheme)
          <*> (cameraFromDesc fn      $ sd^.sceneDescCamera)
          <*> (tracerFromDesc         $ sd^.sceneDescTracer)

worldFromDesc :: MeshGroup -> Int -> WorldDesc -> LoadM World
worldFromDesc mg fn wd =
    World <$> (pure                  $ wd^.wdViewPlane)
          <*> (pure                  $ wd^.wdBgColor)
          <*> (objectsFromDesc mg fn $ wd^.wdObjects)
          <*> (lightsFromDesc mg fn  $ wd^.wdLights)
          <*> (lightFromDesc mg fn   $ wd^.wdAmbient)
          <*> (pure                  $ wd^.wdWorldShadows)

objectsFromDesc :: MeshGroup -> Int -> [ObjectDesc] -> LoadM [Object]
objectsFromDesc mg fn os = concat <$> sequenceA (objectFromDesc mg fn <$> os)

single :: LoadM b -> LoadM [b]
single v = (:[]) <$> v

objectFromDesc :: MeshGroup -> Int -> ObjectDesc -> LoadM [Object]
objectFromDesc _ fn (Sphere c r m) = single $ sphere c r <$> materialFromDesc fn m
objectFromDesc _ fn (ConcaveSphere c r m) = single $ concaveSphere c r <$> materialFromDesc fn m
objectFromDesc _ fn (Rectangle p0 a b dbl m) = single $ rectangle p0 a b dbl <$> materialFromDesc fn m
objectFromDesc _ fn (Triangle v1 v2 v3 m) = single $ tri v1 v2 v3 <$> materialFromDesc fn m
objectFromDesc _ fn (Box v1 v2 m) = single $ box v1 v2 <$> materialFromDesc fn m
objectFromDesc _ fn (Plane c norm m) = single $ plane c norm <$> materialFromDesc fn m
objectFromDesc mg fn (Mesh src m) = do
    mData <- case M.lookup src mg of
               Just md -> return md
               Nothing -> fail $ "Could not find preloaded mesh for " ++ show src
    theMesh <- mesh mData <$> materialFromDesc fn m
    return [theMesh]
objectFromDesc mg fn (Grid os) = single $ grid <$> (concat <$> sequenceA (objectFromDesc mg fn <$> os))
objectFromDesc mg fn (Instances oDesc is) = do
    v <- objectFromDesc mg fn oDesc
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

lightsFromDesc :: MeshGroup -> Int -> [LightDesc] -> LoadM [Light]
lightsFromDesc mg fn ls = sequenceA (lightFromDesc mg fn <$> ls)

lightFromDesc :: MeshGroup -> Int -> LightDesc -> LoadM Light
lightFromDesc _ _ (Ambient s c) = return $ ambientLight s c
lightFromDesc _ _ (AmbientOccluder c min_amt s) = return $ ambientOccluder c min_amt s
lightFromDesc _ _ (Point sh ls c loc) = return $ pointLight sh ls c loc
lightFromDesc _ fn (Environment sh m) = environmentLight sh <$> (materialFromDesc fn m)
lightFromDesc mg fn (Area sh oDesc p) = do
    v <- objectFromDesc mg fn oDesc
    case v of
      [o] -> return $ areaLight sh o p
      _ -> fail "Could not create area light from multiple objects"

accelSchemeFromDesc :: AccelSchemeDesc -> LoadM AccelScheme
accelSchemeFromDesc NoScheme = return noScheme
accelSchemeFromDesc GridScheme = return gridScheme

materialFromDesc :: Int -> MaterialDesc -> LoadM Material
materialFromDesc _ (Matte c) = return $ matteFromColor c
materialFromDesc fn (Mix amt m1 m2) = mix amt <$> materialFromDesc fn m1 <*> materialFromDesc fn m2
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
                   (animate fn $ cd^.thinLensVpDist)
                   (animate fn $ cd^.thinLensFpDist)
                   (animate fn $ cd^.thinLensRadius)
                   <$> (v2SamplerFromDesc $ cd^.thinLensSampler)
