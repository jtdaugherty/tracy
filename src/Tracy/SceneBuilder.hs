module Tracy.SceneBuilder
  ( sceneFromDesc
  )
  where

import Control.Applicative
import Control.Lens ((^.))
import Data.Traversable (sequenceA)

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

import Tracy.Anim
import Tracy.Tracers
import Tracy.AccelSchemes
import Tracy.Samplers

sceneFromDesc :: SceneDesc -> Int -> Either String (Scene ThinLens)
sceneFromDesc sd fn =
    Scene <$> (worldFromDesc fn    $ sd^.sceneDescWorld)
          <*> (accelSchemeFromDesc $ sd^.sceneDescAccelScheme)
          <*> (cameraFromDesc fn   $ sd^.sceneDescCamera)
          <*> (tracerFromDesc      $ sd^.sceneDescTracer)

worldFromDesc :: Int -> WorldDesc -> Either String World
worldFromDesc fn wd =
    World <$> (pure               $ wd^.wdViewPlane)
          <*> (pure               $ wd^.wdBgColor)
          <*> (objectsFromDesc fn $ wd^.wdObjects)
          <*> (lightsFromDesc fn  $ wd^.wdLights)
          <*> (lightFromDesc fn   $ wd^.wdAmbient)
          <*> (pure               $ wd^.wdWorldShadows)

objectsFromDesc :: Int -> [ObjectDesc] -> Either String [Object]
objectsFromDesc fn os = concat <$> sequenceA (objectFromDesc fn <$> os)

single :: Either a b -> Either a [b]
single v = (:[]) <$> v

objectFromDesc :: Int -> ObjectDesc -> Either String [Object]
objectFromDesc fn (Sphere c r m) = single $ sphere c r <$> materialFromDesc fn m
objectFromDesc fn (ConcaveSphere c r m) = single $ concaveSphere c r <$> materialFromDesc fn m
objectFromDesc fn (Rectangle p0 a b m) = single $ rectangle p0 a b <$> materialFromDesc fn m
objectFromDesc fn (Triangle v1 v2 v3 m) = single $ tri v1 v2 v3 <$> materialFromDesc fn m
objectFromDesc fn (Box v1 v2 m) = single $ box v1 v2 <$> materialFromDesc fn m
objectFromDesc fn (Plane c norm m) = single $ plane c norm <$> materialFromDesc fn m
objectFromDesc fn (Mesh mDesc m) = single $ mesh mDesc <$> materialFromDesc fn m
objectFromDesc fn (Grid os) = single $ grid <$> (concat <$> sequenceA (objectFromDesc fn <$> os))
objectFromDesc fn (Instances oDesc pairs) =
    let mkInstance o (mMatrix, mMat) =
            case mMat of
                Nothing -> Right $ inst mMatrix Nothing o
                Just mDesc -> inst mMatrix <$> (Just <$> materialFromDesc fn mDesc) <*> (pure o)
    in case objectFromDesc fn oDesc of
          Left e -> Left e
          Right [o] -> sequenceA (mkInstance o <$> pairs)
          Right _ -> error "Error: Instances of (multiple) Instances not allowed"

lightsFromDesc :: Int -> [LightDesc] -> Either String [Light]
lightsFromDesc fn ls = sequenceA (lightFromDesc fn <$> ls)

lightFromDesc :: Int -> LightDesc -> Either String Light
lightFromDesc _ (Ambient s c) = Right $ ambientLight s c
lightFromDesc _ (AmbientOccluder c min_amt s) = Right $ ambientOccluder c min_amt s
lightFromDesc _ (Point sh ls c loc) = Right $ pointLight sh ls c loc
lightFromDesc fn (Environment sh m) = environmentLight sh <$> (materialFromDesc fn m)
lightFromDesc fn (Area sh oDesc p) =
    case objectFromDesc fn oDesc of
      Left e -> Left e
      Right [o] -> Right $ areaLight sh o p
      Right _ -> Left "Could not create area light from multiple objects"

accelSchemeFromDesc :: AccelSchemeDesc -> Either String AccelScheme
accelSchemeFromDesc NoScheme = Right noScheme
accelSchemeFromDesc GridScheme = Right gridScheme

materialFromDesc :: Int -> MaterialDesc -> Either String Material
materialFromDesc _ (Matte c) = Right $ matteFromColor c
materialFromDesc _ (Phong c ks e) = Right $ phongFromColor c ks e
materialFromDesc _ (Reflective c ks e cr kr) = Right $ reflective c ks e cr kr
materialFromDesc _ (GlossyReflective c ks e cr kr er) = Right $ glossyReflective c ks e cr kr er
materialFromDesc _ (Emissive c e) = Right $ emissive c e

tracerFromDesc :: TracerDesc -> Either String Tracer
tracerFromDesc RayCastTracer = Right rayCastTracer
tracerFromDesc AreaLightTracer = Right areaLightTracer
tracerFromDesc WhittedTracer = Right whittedTracer

cameraFromDesc :: Int -> CameraDesc -> Either String (Camera ThinLens)
cameraFromDesc fn cd@(ThinLensCamera { }) =
    Right $ thinLensCamera (animV3 fn $ cd^.thinLensEye)
                           (cd^.thinLensLookAt)
                           (cd^.thinLensUp)
                           (cd^.thinLensExposure)
                           (cd^.thinLensZ)
                           (cd^.thinLensVpDist)
                           (cd^.thinLensFpDist)
                           (animFloat fn $ cd^.thinLensRadius)
                           (toUnitDisk multiJittered)
