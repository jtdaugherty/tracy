module Tracy.SceneBuilder
  ( sceneFromDesc
  )
  where

import Control.Applicative
import Control.Lens ((^.))
import Data.Traversable (sequenceA)

import Tracy.Types
import Tracy.Cameras

import Tracy.Materials.Phong
import Tracy.Materials.Matte

import Tracy.Objects.Sphere
import Tracy.Objects.Box
import Tracy.Objects.Plane
import Tracy.Objects.Triangle
import Tracy.Objects.Mesh
import Tracy.Objects.Instance

import Tracy.Lights.Ambient
import Tracy.Lights.AmbientOccluder
import Tracy.Lights.Point

import Tracy.Tracers
import Tracy.AccelSchemes
import Tracy.Samplers

sceneFromDesc :: SceneDesc -> Either String (Scene ThinLens)
sceneFromDesc sd =
    Scene <$> (worldFromDesc       $ sd^.sceneDescWorld)
          <*> (accelSchemeFromDesc $ sd^.sceneDescAccelScheme)
          <*> (cameraFromDesc      $ sd^.sceneDescCamera)
          <*> (tracerFromDesc      $ sd^.sceneDescTracer)

worldFromDesc :: WorldDesc -> Either String World
worldFromDesc wd =
    World <$> (pure              $ wd^.wdViewPlane)
          <*> (pure              $ wd^.wdBgColor)
          <*> (objectsFromDesc   $ wd^.wdObjects)
          <*> (lightsFromDesc    $ wd^.wdLights)
          <*> (lightFromDesc     $ wd^.wdAmbient)
          <*> (pure              $ wd^.wdWorldShadows)

objectsFromDesc :: [ObjectDesc] -> Either String [Object]
objectsFromDesc os = concat <$> sequenceA (objectFromDesc <$> os)

single :: Either a b -> Either a [b]
single v = (:[]) <$> v

objectFromDesc :: ObjectDesc -> Either String [Object]
objectFromDesc (Sphere c r m) = single $ sphere c r <$> materialFromDesc m
objectFromDesc (Triangle v1 v2 v3 m) = single $ tri v1 v2 v3 <$> materialFromDesc m
objectFromDesc (Box v1 v2 m) = single $ box v1 v2 <$> materialFromDesc m
objectFromDesc (Plane c norm m) = single $ plane c norm <$> materialFromDesc m
objectFromDesc (Mesh mDesc m) = single $ mesh mDesc <$> materialFromDesc m
objectFromDesc (Grid os) = single $ grid <$> (concat <$> sequenceA (objectFromDesc <$> os))
objectFromDesc (Instances oDesc pairs) =
    let mkInstance o (mMatrix, mMat) =
            case mMat of
                Nothing -> Right $ inst mMatrix Nothing o
                Just mDesc -> inst mMatrix <$> (Just <$> materialFromDesc mDesc) <*> (pure o)
    in case objectFromDesc oDesc of
          Left e -> Left e
          Right [o] -> sequenceA (mkInstance o <$> pairs)
          Right _ -> error "Error: Instances of (multiple) Instances not allowed"

lightsFromDesc :: [LightDesc] -> Either String [Light]
lightsFromDesc ls = sequenceA (lightFromDesc <$> ls)

lightFromDesc :: LightDesc -> Either String Light
lightFromDesc (Ambient s c) = Right $ ambientLight s c
lightFromDesc (AmbientOccluder c min_amt s) = Right $ ambientOccluder c min_amt s
lightFromDesc (Point sh ls c loc) = Right $ pointLight sh ls c loc

accelSchemeFromDesc :: AccelSchemeDesc -> Either String AccelScheme
accelSchemeFromDesc NoScheme = Right noScheme
accelSchemeFromDesc GridScheme = Right gridScheme

materialFromDesc :: MaterialDesc -> Either String Material
materialFromDesc (Matte c) = Right $ matteFromColor c
materialFromDesc (Phong c e) = Right $ phongFromColor c e

tracerFromDesc :: TracerDesc -> Either String Tracer
tracerFromDesc RayCastTracer = Right rayCastTracer

cameraFromDesc :: CameraDesc -> Either String (Camera ThinLens)
cameraFromDesc cd@(ThinLensCamera { }) =
    Right $ thinLensCamera (cd^.thinLensEye)
                           (cd^.thinLensLookAt)
                           (cd^.thinLensUp)
                           (cd^.thinLensExposure)
                           (cd^.thinLensZ)
                           (cd^.thinLensVpDist)
                           (cd^.thinLensFpDist)
                           (cd^.thinLensRadius)
                           (toUnitDisk jittered)
