module Tracy.SceneBuilder
  ( sceneFromDesc
  )
  where

import Control.Lens ((^.))
import qualified Data.Map as M
import qualified Data.Vector as V

import Tracy.Types
import Tracy.Cameras.ThinLens

import Tracy.Materials.Emissive
import Tracy.Materials.Phong
import Tracy.Materials.Matte
import Tracy.Materials.Mix
import Tracy.Materials.Add

import Tracy.Objects.Sphere
import Tracy.Objects.Torus
import Tracy.Objects.Box
import Tracy.Objects.Grid
import Tracy.Objects.BVH
import Tracy.Objects.Plane
import Tracy.Objects.Rectangle
import Tracy.Objects.Triangle
import Tracy.Objects.Mesh
import Tracy.Objects.Instance

import Tracy.Textures.ConstantColor
import Tracy.Textures.TransformedTexture
import Tracy.Textures.ImageTexture
import Tracy.Textures.PlaneChecker
import Tracy.Textures.SphereChecker
import Tracy.TextureMapping.Spherical
import Tracy.TextureMapping.Tile

import Tracy.Lights.Ambient
import Tracy.Lights.AmbientOccluder
import Tracy.Lights.Area
import Tracy.Lights.Environment
import Tracy.Lights.Point

import Tracy.Anim ()
import Tracy.Tracers
import Tracy.Samplers
import Tracy.Transformations

type LoadM a = Either String a

sceneFromDesc :: SceneDesc -> ImageGroup -> MeshGroup -> Frame -> Either String (Scene ThinLens)
sceneFromDesc sd ig mg fn = runSceneFromDesc sd ig mg fn

runSceneFromDesc :: SceneDesc -> ImageGroup -> MeshGroup -> Frame -> LoadM (Scene ThinLens)
runSceneFromDesc sd ig mg fn =
    Scene <$> (worldFromDesc ig mg fn $ sd^.sceneDescWorld)
          <*> (cameraFromDesc fn      $ sd^.sceneDescCamera)
          <*> (tracerFromDesc         $ sd^.sceneDescTracer)

worldFromDesc :: ImageGroup -> MeshGroup -> Frame -> WorldDesc -> LoadM World
worldFromDesc ig mg fn wd =
    World <$> (viewPlaneFromDesc fn     $ wd^.wdViewPlane)
          <*> (pure                     $ wd^.wdBgColor)
          <*> (objectsFromDesc ig mg fn $ wd^.wdObjects)
          <*> (lightsFromDesc ig mg fn  $ wd^.wdLights)
          <*> (lightFromDesc ig mg fn   $ wd^.wdAmbient)
          <*> (pure                     $ wd^.wdWorldShadows)

viewPlaneFromDesc :: Frame -> ViewPlaneDesc -> LoadM ViewPlane
viewPlaneFromDesc _ vpd =
    ViewPlane (vpd^.vpHres)
        (vpd^.vpVres)
        (vpd^.vpPixelSize)
        (vpd^.vpGamma)
        (vpd^.vpInverseGamma)
        (vpd^.vpMaxDepth)
        <$> (v2SamplerFromDesc $ vpd^.vpPixelSampler)

objectsFromDesc :: ImageGroup -> MeshGroup -> Frame -> [ObjectDesc] -> LoadM [Object]
objectsFromDesc ig mg fn os = concat <$> sequenceA (objectFromDesc ig mg fn <$> os)

single :: LoadM b -> LoadM [b]
single v = (:[]) <$> v

objectFromDesc :: ImageGroup -> MeshGroup -> Frame -> ObjectDesc -> LoadM [Object]
objectFromDesc ig _ fn (Sphere m) =
    single $ sphere <$> materialFromDesc ig fn m Nothing
objectFromDesc ig _ fn (Torus r1 r2 m) =
    single $ torus r1 r2 <$> materialFromDesc ig fn m Nothing
objectFromDesc ig _ fn (ConcaveSphere m) =
    single $ concaveSphere <$> materialFromDesc ig fn m Nothing
objectFromDesc ig _ fn (Rectangle dbl m) =
    single $ rectangle dbl <$> materialFromDesc ig fn m Nothing
objectFromDesc ig _ fn (Triangle v1 v2 v3 m) =
    single $ tri v1 v2 v3 <$> materialFromDesc ig fn m Nothing
objectFromDesc ig _ fn (Box m) =
    single $ box <$> materialFromDesc ig fn m Nothing
objectFromDesc ig _ fn (Plane m) =
    single $ plane <$> materialFromDesc ig fn m Nothing
objectFromDesc ig mg fn (Mesh src m) = do
    mData <- case M.lookup src mg of
               Just md -> return md
               Nothing -> Left $ "Could not find preloaded mesh for " ++ show src
    theMesh <- mesh mData <$> materialFromDesc ig fn m Nothing
    return [theMesh]
objectFromDesc ig mg fn (Grid os) =
    single $ grid <$> V.fromList <$> (concat <$> sequenceA (objectFromDesc ig mg fn <$> os))
objectFromDesc ig mg fn (BVH os) =
    single $ bvh <$> (concat <$> sequenceA (objectFromDesc ig mg fn <$> os))
objectFromDesc ig mg fn (Instances oDesc is) = do
    v <- objectFromDesc ig mg fn oDesc
    ids <- sequenceA $ instanceDataFromDesc ig fn <$> is
    case v of
        [o] -> return $ (\(t, m) -> inst t m o) <$> ids
        _ -> error "Error: Instances of (multiple) Instances not allowed"

instanceDataFromDesc :: ImageGroup -> Frame -> InstanceDesc -> LoadM (Transformation, Maybe Material)
instanceDataFromDesc ig fn (ID tDesc mDesc) = do
    ts <- sequenceA (transformationFromDesc <$> tDesc)
    mResult <- case mDesc of
                   Nothing -> return Nothing
                   Just md -> Just <$> materialFromDesc ig fn md (Just $ mconcat ts)
    return (mconcat ts, mResult)

transformationFromDesc :: TransformationDesc -> LoadM Transformation
transformationFromDesc (Translate x y z) = return $ translate x y z
transformationFromDesc (Scale x y z) = return $ scale x y z
transformationFromDesc (ScaleUni f) = return $ scaleUni f
transformationFromDesc (RotateX v) = return $ rotateX v
transformationFromDesc (RotateY v) = return $ rotateY v
transformationFromDesc (RotateZ v) = return $ rotateZ v

lightsFromDesc :: ImageGroup -> MeshGroup -> Frame -> [LightDesc] -> LoadM [Light]
lightsFromDesc ig mg fn ls = sequenceA (lightFromDesc ig mg fn <$> ls)

lightFromDesc :: ImageGroup -> MeshGroup -> Frame -> LightDesc -> LoadM Light
lightFromDesc _ _ _ (Ambient s c) =
    return $ ambientLight s c
lightFromDesc _ _ _ (AmbientOccluder c min_amt s) =
    return $ ambientOccluder c min_amt s
lightFromDesc _ _ _ (Point sh ls c loc) =
    return $ pointLight sh ls c loc
lightFromDesc ig _ fn (Environment sh m) =
    environmentLight sh <$> (materialFromDesc ig fn m Nothing)
lightFromDesc ig mg fn (Area sh oDesc p) = do
    v <- objectFromDesc ig mg fn oDesc
    case v of
      [o] -> return $ areaLight sh o p
      _ -> Left "Could not create area light from multiple objects"

materialFromDesc :: ImageGroup -> Frame -> MaterialDesc -> Maybe Transformation -> LoadM Material
materialFromDesc ig _ (Matte td) trans =
    matteFromTexture <$> textureFromDesc ig td trans
materialFromDesc ig fn (Mix amt m1 m2) trans =
    mix (animate fn amt) <$> materialFromDesc ig fn m1 trans
                         <*> materialFromDesc ig fn m2 trans
materialFromDesc ig fn (Add m1 m2) trans =
    add <$> materialFromDesc ig fn m1 trans
        <*> materialFromDesc ig fn m2 trans
materialFromDesc ig _ (Phong t ks e) trans =
    phongFromColor <$> textureFromDesc ig t trans
                   <*> pure ks
                   <*> pure e
materialFromDesc ig _ (Reflective td ks e tr kr) trans =
    reflective <$> textureFromDesc ig td trans
               <*> pure ks
               <*> pure e
               <*> textureFromDesc ig tr trans
               <*> pure kr
materialFromDesc ig _ (GlossyReflective td ks e tr kr er) trans =
    glossyReflective <$> textureFromDesc ig td trans
                     <*> pure ks
                     <*> pure e
                     <*> textureFromDesc ig tr trans
                     <*> pure kr
                     <*> pure er
materialFromDesc _ _ (Emissive c e) _ = return $ emissive c e

textureFromDesc :: ImageGroup -> TextureDesc -> Maybe Transformation -> LoadM Texture
textureFromDesc _ (ConstantColor c) _ = return $ constantColor c
textureFromDesc _ (PlaneChecker sz) trans =
    case trans of
        Nothing -> return $ planeChecker sz
        Just tr -> return $ transformedTexture tr $ planeChecker sz
textureFromDesc _ (SphereChecker cnt) trans = do
    case trans of
        Nothing -> return $ sphereChecker cnt
        Just tr -> return $ transformedTexture tr $ sphereChecker cnt
textureFromDesc ig (ImageTexture fp mappingDesc) trans = do
    img <- case M.lookup fp ig of
        Just img -> return img
        Nothing -> Left $ "Could not find image at path " ++ show fp
    mapping <- case mappingDesc of
        Nothing -> return Nothing
        Just md -> Just <$> mappingFromDesc md
    case trans of
        Nothing -> return $ imageTexture mapping img
        Just tr -> return $ transformedTexture tr $ imageTexture mapping img

mappingFromDesc :: MappingDesc -> LoadM TextureMapping
mappingFromDesc Spherical = return sphericalMapping
mappingFromDesc (Tile s) = return $ tileMapping s

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

cameraFromDesc :: Frame -> CameraDesc -> LoadM (Camera ThinLens)
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
