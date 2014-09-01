module Tracy.SceneBuilder
  ( sceneFromDesc
  )
  where

import Control.Applicative
import Control.Lens ((^.))
import Data.Traversable (sequenceA)

import Tracy.Types
import Tracy.Cameras
import Tracy.Materials

import Tracy.Sphere
import Tracy.Box
import Tracy.Plane
import Tracy.Triangle

import Tracy.Lights
import Tracy.AccelSchemes
import Tracy.Samplers

sceneFromDesc :: SceneDesc -> Either String (Scene ThinLens)
sceneFromDesc sd =
    Scene <$> (worldFromDesc       $ sd^.sceneDescWorld)
          <*> (accelSchemeFromDesc $ sd^.sceneDescAccelScheme)
          <*> (cameraFromDesc      $ sd^.sceneDescCamera)

worldFromDesc :: WorldDesc -> Either String World
worldFromDesc wd =
    World <$> (pure              $ wd^.wdViewPlane)
          <*> (pure              $ wd^.wdBgColor)
          <*> (objectsFromDesc   $ wd^.wdObjects)
          <*> (lightsFromDesc    $ wd^.wdLights)
          <*> (lightFromDesc     $ wd^.wdAmbient)
          <*> (pure              $ wd^.wdWorldShadows)

objectsFromDesc :: [ObjectDesc] -> Either String [Object]
objectsFromDesc os = sequenceA (objectFromDesc <$> os)

objectFromDesc :: ObjectDesc -> Either String Object
objectFromDesc (Sphere c r m) = sphere c r <$> materialFromDesc m
objectFromDesc (Triangle v1 v2 v3 m) = tri v1 v2 v3 <$> materialFromDesc m
objectFromDesc (Box v1 v2 m) = box v1 v2 <$> materialFromDesc m
objectFromDesc (Plane c norm m) = plane c norm <$> materialFromDesc m

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
