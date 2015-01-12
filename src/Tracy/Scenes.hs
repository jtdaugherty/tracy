{-# LANGUAGE TemplateHaskell #-}
module Tracy.Scenes
  ( allScenes
  ) where

import Control.Lens
import Data.Colour
import Data.Monoid
import Linear

import Tracy.Types
import Tracy.Transformations
import Tracy.Objects.Mesh

defaultVp :: ViewPlane
defaultVp =
    ViewPlane { _hres = 800
              , _vres = 800
              , _pixelSize = 1.0
              , _gamma = 1.0
              , _inverseGamma = 1.0
              , _maxDepth = 5
              }

defCamera :: CameraDesc
defCamera =
    ThinLensCamera (Val $ V3 0 100 300)
                   (V3 0 0 0)
                   (V3 0 1 0)
                   1.0
                   1.0
                   500 -- vpDist
                   300 -- fpDist
                   0

world :: [ObjectDesc] -> [LightDesc] -> WorldDesc
world os ls = WorldDesc { _wdViewPlane = defaultVp
                        , _wdObjects = os
                        , _wdBgColor = cBlack
                        , _wdLights = ls
                        , _wdAmbient = Ambient 1 cWhite
                        , _wdWorldShadows = True
                        }

worldOcc :: [ObjectDesc] -> [LightDesc] -> Float -> WorldDesc
worldOcc os ls ambStr = WorldDesc { _wdViewPlane = defaultVp
                                  , _wdObjects = os
                                  , _wdBgColor = cBlack
                                  , _wdLights =  ls
                                  , _wdAmbient = AmbientOccluder cWhite cBlack ambStr
                                  , _wdWorldShadows = True
                                  }

oneSphere :: SceneDesc
oneSphere =
    let s = Sphere (V3 (-40) 0 0) 85.0 (Phong cRed 0.5 100)
        ls = [ Point True 1 cWhite (V3 (-500) 500 500)
             ]
    in SceneDesc (world [s] ls) NoScheme defCamera RayCastTracer

instancedSpheres :: SceneDesc
instancedSpheres =
    let s = Sphere (V3 0 0 0) 40.0 (Phong cRed 0.5 100)
        p = Plane (V3 0 (-100) 0) (V3 0 1 0) (Matte cWhite)
        ls = [ Point False 1 cWhite (V3 (-500) 500 500)
             ]
        is = Instances s [ (translate (-50) 50 0     , Just $ Phong cBlue  0.5 50)
                         , (translate (-50) (-50) 0  , Just $ Phong cWhite 0.5 50)
                         , (translate 50 50 0        , Just $ Phong cRed   0.5 50)
                         , (translate 50 (-50) 0     , Just $ Phong cGreen 0.5 50)
                         ]
    in SceneDesc (worldOcc [is, p] ls 1) NoScheme
         (defCamera & thinLensLookAt .~ (V3 0 0 0)
                    & thinLensEye    .~ (Val $ V3 0 0 200)
                    )
         RayCastTracer

instancedSpheresGrid :: SceneDesc
instancedSpheresGrid =
    let s = Sphere (V3 0 0 0) 40.0 (Phong cRed 0.5 100)
        p = Plane (V3 0 (-100) 0) (V3 0 1 0) (Matte cWhite)
        ls = [ Point False 1 cWhite (V3 (-500) 500 500)
             ]
        g = Grid [ Instances s [ (translate (-50) 50 0     , Just $ Phong cBlue  0.5 50)
                               , (translate (-50) (-50) 0  , Just $ Phong cWhite 0.5 50)
                               , (translate 50 50 0        , Just $ Phong cRed   0.5 50)
                               , (translate 50 (-50) 0     , Just $ Phong cGreen 0.5 50)
                               ]
                 ]
    in SceneDesc (worldOcc [g, p] ls 1) NoScheme
         (defCamera & thinLensLookAt .~ (V3 0 0 0)
                    & thinLensEye    .~ (Val $ V3 0 0 200)
                    )
         RayCastTracer

objectDemo :: SceneDesc
objectDemo =
    let s = Sphere (V3 0 0 11) 30.0 (Matte cBlue)
        p = Plane (V3 0 0 0) (V3 0 1 0) (Matte cGreen)
        s2 = Sphere (V3 50 5 0) 10.0 (Matte cMagenta)
        s3 = Sphere (V3 (-50) 10 0) 15.0 (Matte cYellow)
        b1 = Box (V3 100 0 0) (V3 150 50 50) (Matte cCyan)
        b2 = Box (V3 100 0 (-100)) (V3 150 50 (-50)) (Matte cRed)
        b3 = Box (V3 (-150) 0 25) (V3 (-100) 75 75) (Matte cYellow)
        b4 = Box (V3 (-150) 0 (-75)) (V3 (-100) 75 (-25)) (Matte cWhite)
        t1 = Triangle (V3 100 50 0) (V3 50 100 0) (V3 (-50) 75 0) (Matte cWhite)
        ls = [ Point True 1 cWhite (V3 (-500) 500 500)
             ]
    in SceneDesc (worldOcc [t1, s, p, s2, s3, b1, b2, b3, b4] ls 3) NoScheme
         (defCamera & thinLensEye .~ (LerpRotY (1, 100) (V3 0 100 300) (2 * pi))
         )
         RayCastTracer

objectDemoNoPoint :: SceneDesc
objectDemoNoPoint =
    let s = Sphere (V3 0 0 11) 30.0 (Matte cBlue)
        p = Plane (V3 0 0 0) (V3 0 1 0) (Matte cGreen)
        s2 = Sphere (V3 50 5 0) 10.0 (Matte cMagenta)
        s3 = Sphere (V3 (-50) 10 0) 15.0 (Matte cYellow)
        b1 = Box (V3 100 0 0) (V3 150 50 50) (Matte cCyan)
        b2 = Box (V3 100 0 (-100)) (V3 150 50 (-50)) (Matte cRed)
        b3 = Box (V3 (-150) 0 25) (V3 (-100) 75 75) (Matte cYellow)
        b4 = Box (V3 (-150) 0 (-75)) (V3 (-100) 75 (-25)) (Matte cWhite)
        t1 = Triangle (V3 100 50 0) (V3 50 100 0) (V3 (-50) 75 0) (Matte cWhite)
    in SceneDesc (worldOcc [t1, s, p, s2, s3, b1, b2, b3, b4] [] 4) NoScheme defCamera RayCastTracer

sphereGrid :: [ObjectDesc]
sphereGrid =
    let spheres = concat [ ss y e r | (y, e, r) <- params ]
        params = [ (-1200, 1   , 30.0)
                 , (-1100, 1   , 20.0)
                 , (-1000, 1   , 10.0)
                 , (-900 , 1   , 20.0)
                 , (-800 , 1   , 30.0)
                 , (-700 , 1   , 40.0)
                 , (-600 , 2   , 30.0)
                 , (-500 , 1   , 10.0)
                 , (-400 , 10  , 20.0)
                 , (-300 , 10  , 30.0)
                 , (-200 , 20  , 15.0)
                 , (-100 , 75  , 20.0)
                 , (0    , 200 , 25.0)
                 , (100  , 75  , 30.0)
                 , (200  , 20  , 35.0)
                 , (300  , 10  , 10.0)
                 , (400  , 10  , 30.0)
                 , (500  , 1   , 20.0)
                 ]
        pairs = [ (-1050, (Phong cBlue))
                , (-1000, (Phong cYellow))
                , (-950, (Phong cGreen))
                , (-900, (Phong cWhite))
                , (-850, (Phong cBlue))
                , (-800, (Phong cMagenta))
                , (-750, (Phong cCyan))
                , (-700, (Phong cYellow))
                , (-650, (Phong cBlue))
                , (-600, (Phong cGreen))
                , (-550, (Phong cWhite))
                , (-500, (Phong cRed))
                , (-450, (Phong cMagenta))
                , (-400, (Phong cBlue))
                , (-350, (Phong cGreen))
                , (-300, (Phong cWhite))
                , (-250, (Phong cYellow))
                , (-200, (Phong cCyan))
                , (-150, (Phong cRed))
                , (-100, (Phong cMagenta))
                , (-50, (Phong cBlue))
                , (0, (Phong cGreen))
                , (50, (Phong cWhite))
                , (100, (Phong cYellow))
                , (150, (Phong cMagenta))
                ]
        ss y e r = [ Sphere (V3 xz y xz) r (mkMat 0.5 e)
                   | (xz, mkMat) <- pairs
                   ]
    in spheres

clearSpheres :: SceneDesc
clearSpheres =
    let ls = [ Point False 1.5 cWhite (V3 (-500) 500 500)
             ]
    in SceneDesc (world sphereGrid ls & wdWorldShadows .~ False)
                 GridScheme
                 defCamera
                 RayCastTracer

blurrySpheres :: SceneDesc
blurrySpheres =
    let ls = [ Point False 1.5 cWhite (V3 (-500) 500 500)
             ]
    in SceneDesc (worldOcc sphereGrid ls 1 & wdWorldShadows .~ False)
                 GridScheme
                 (defCamera & thinLensRadius .~ 10.0
                            & thinLensLookAt .~ (V3 0 30 0)
                            & thinLensVpDist .~ 500
                            & thinLensFpDist .~ 500
                            & thinLensEye    .~ (Val $ V3 0 50 300))
                 RayCastTracer

loadCubeScene :: IO SceneDesc
loadCubeScene = do
    mDesc <- loadMesh "meshes/cube.ply"
    let cObj = Mesh mDesc $ Matte cWhite
        ls = [ Point True 1 cWhite (V3 (-10) 10 10)
             ]
    return $ SceneDesc (world [cObj] ls) NoScheme
                 (defCamera & thinLensRadius .~ 0.0
                            & thinLensLookAt .~ (V3 0 0 0)
                            & thinLensVpDist .~ 200
                            & thinLensFpDist .~ 200
                            & thinLensEye    .~ (Val $ V3 2.1 2.1 3)
                            )
                 RayCastTracer

loadBunnyScene :: IO SceneDesc
loadBunnyScene = do
    mDesc <- loadMesh "meshes/bunny.ply"
    let cObj = Mesh mDesc $ Phong cWhite 0.5 100
        p = Plane (V3 0 0 0.1) (V3 0 0 1) (Matte cGreen)
        ls = [ Point True 1 cWhite (V3 (-5) 5 5)
             , Point True 1 cWhite (V3 5 5 5)
             , Point True 1 cWhite (V3 5 (-5) 5)
             , Point True 1 cWhite (V3 5 (-5) (-5))
             , Point True 1 cWhite (V3 5 0 2)
             ]
    return $ SceneDesc (worldOcc [cObj, p] ls 1) NoScheme
                 (defCamera & thinLensRadius .~ 0.0
                            & thinLensLookAt .~ (V3 0 0 2)
                            & thinLensVpDist .~ 300
                            & thinLensFpDist .~ 300
                            & thinLensUp     .~ (V3 0 0 1)
                            & thinLensEye    .~ (Val $ V3 (-6) (-3) 3)
                            )
                 RayCastTracer

loadTableScene :: IO SceneDesc
loadTableScene = do
    tableDesc <- loadMesh "meshes/table.ply"
    monkeyDesc <- loadMesh "meshes/monkey2.ply"
    icoDesc <- loadMesh "meshes/ico1.ply"
    torusDesc <- loadMesh "meshes/torus.ply"
    chairLDesc <- loadMesh "meshes/chairLeft.ply"
    chairRDesc <- loadMesh "meshes/chairRight.ply"

    let tableObj =  Mesh tableDesc  $ Phong (Colour (100.0/255.0) (73.0/255.0) 0) 0.5 100
        monkeyObj = Mesh monkeyDesc $ GlossyReflective cCyan 0.7 10 cWhite 0.7 10
        torusObj =  Mesh torusDesc  $ Reflective (Colour 1 0.7 0.7) 0.9 1000 cWhite 0.7
        icoObj =   Mesh icoDesc     $ Matte cYellow
        sphereObj = Sphere (V3 0.10225 4.449909 (-1.18322)) 0.811 $ Reflective (grey 0.3) 0.9 1000 cWhite 0.9
        chairLeft = Mesh chairLDesc $ Phong (Colour 1 (79/255) 0) 1.0 100000
        chairRight = Mesh chairRDesc $ Phong (Colour 1 (79/255) 0) 1.0 100000

        a = Rectangle (V3 (-2.5) 10 0) (V3 5 0 0) (V3 0 0 5) (Emissive (Colour 1 (250/255) (151/255)) 3)
        p = Plane (V3 0 0 0) (V3 0 1 0) (Matte cWhite)
        ls = [ Area True a $ Just 15000
             , Environment True $ Emissive cBlue 1
             ]
    return $ SceneDesc (worldOcc [chairLeft, chairRight, tableObj, monkeyObj, torusObj, icoObj, sphereObj, a, p] ls 1) NoScheme
                 (defCamera & thinLensRadius .~ 0.0
                            & thinLensLookAt .~ (V3 0 3 0)
                            & thinLensVpDist .~ 500
                            & thinLensFpDist .~ 500
                            & thinLensEye .~ (Val $ V3 0 5 3.5)
                            )
                 RayCastTracer

loadDragonScene :: IO SceneDesc
loadDragonScene = do
    mDesc <- loadMesh "meshes/dragon.ply"
    let cObj = Mesh mDesc $ Phong cRed 0.5 50
        p = Plane (V3 0 0.002 0) (V3 0 1 0) (Matte cWhite)
        ls = [ Point True 1 cWhite (V3 0 2 0)
             ]
    return $ SceneDesc (worldOcc [cObj, p] ls 1) NoScheme
                 (defCamera & thinLensRadius .~ 0.0
                            & thinLensLookAt .~ (V3 0 0.15 0)
                            & thinLensVpDist .~ 300
                            & thinLensFpDist .~ 300
                            & thinLensEye    .~ (Val $ V3 (-0.1) 0.1 0.2)
                            )
                 RayCastTracer

loadMonkeyScene :: IO SceneDesc
loadMonkeyScene = do
    mDesc <- loadMesh "meshes/monkey.ply"
    let cObj = Mesh mDesc $ Phong cWhite 0.5 50
        ls = [ Point True 1 cWhite (V3 (-10) 10 10)
             ]
    return $ SceneDesc (worldOcc [cObj] ls 1) NoScheme
                 (defCamera & thinLensRadius .~ 0.0
                            & thinLensLookAt .~ (V3 0 0 0)
                            & thinLensVpDist .~ 300
                            & thinLensFpDist .~ 300
                            & thinLensEye    .~ (Val $ V3 0 0 4)
                            )
                 RayCastTracer

rectangles :: SceneDesc
rectangles =
    let r1 = Rectangle (V3 10 0 0) (V3 100 0 0) (V3 0 130 0)     (Phong cBlue  0.5 50)
        r2 = Rectangle (V3 (-120) 0 0) (V3 100 0 0) (V3 0 110 0) (Phong cGreen 0.5 50)
        p = Plane (V3 0 0 0) (V3 0 1 0) (Matte cWhite)
        ls = [ Point True 2 cWhite (V3 (-500) 500 500)
             ]
    in SceneDesc (world [r1, r2, p] ls) NoScheme defCamera RayCastTracer

areaLightScene :: SceneDesc
areaLightScene =
    let p = Plane (V3 0 0 0) (V3 0 1 0) (Matte cWhite)

        a = Rectangle (V3 0 0 0) (V3 50 0 0) (V3 0 50 0) (Matte cWhite)

        i1 = Instances a [(translate (-100) 30 (-100) <> rotateX (pi/6), Just $ Emissive cRed 3)]
        i2 = Instances a [(translate (-10) 30 (-100) <> rotateX (pi/4) , Just $ Emissive cGreen 3)]
        i3 = Instances a [(translate 110 30 (-100) <> rotateX (pi/3)   , Just $ Emissive cBlue 3)]

        ls = [ Area True i1 Nothing
             , Area True i2 Nothing
             , Area True i3 Nothing
             ]
    in SceneDesc (worldOcc [Grid [i1, i2, i3], p] ls 1) NoScheme defCamera AreaLightTracer

reflScene :: SceneDesc
reflScene =
    let s = Sphere (V3 0 40 0) 40 (GlossyReflective cWhite 0 50 cWhite 0.9 50)
        p = Plane (V3 0 0 0) (V3 0 1 0) (GlossyReflective cWhite 0.5 10 cWhite 1 100)

        r1 = Rectangle (V3 (-100) 0 0) (V3 200 0 0) (V3 0 100 0) (Matte cWhite)
        ss = Instances s [ (translate (-90) 0 0,     Just $ GlossyReflective cBlue    0.5 10 cWhite 0.8 10)
                         , (translate 0 0 0,         Just $ GlossyReflective cWhite   0.8 50 cWhite 0.9 500)
                         , (translate 90 0 0,        Just $ GlossyReflective cYellow  1 500 cWhite 0.8 100000)
                         , (translate (-90) 0 90,    Just $ GlossyReflective cRed     0.5 10 cWhite 0.8 10)
                         , (translate 0 0 90,        Just $ GlossyReflective cGreen   0.8 50 cWhite 0.9 500)
                         , (translate 90 0 90,       Just $ GlossyReflective cMagenta 1 500 cWhite 0.8 100000)
                         , (translate (-90) 0 (-90), Just $ GlossyReflective cYellow  0.5 10 cWhite 0.9 10)
                         , (translate 0 0 (-90),     Just $ GlossyReflective cBlack   0.8 50 cWhite 0.9 500)
                         , (translate 90 0 (-90),    Just $ GlossyReflective cCyan    1 500 cWhite 0.9 100000)
                         ]
        i1 = Instances r1 [ (translate 0 300 0 <> rotateX (pi/(2.5)), Just $ Emissive cWhite 3)
                          ]
        ls = [ Area True i1 $ Just 200000
             ]
    in SceneDesc (worldOcc [Grid [ss, i1], p] ls 1) NoScheme
                 (defCamera & thinLensEye .~ (LerpRotY (1, 100) (V3 200 250 250) (2 * pi))
                 )
                 AreaLightTracer

allScenes :: [(String, IO SceneDesc)]
allScenes =
    [ ("one-sphere",      return oneSphere)
    , ("instanced-spheres", return instancedSpheres)
    , ("instanced-spheres-grid", return instancedSpheresGrid)
    , ("object-demo",     return objectDemo)
    , ("object-demo2",    return objectDemoNoPoint)
    , ("clear-spheres",   return clearSpheres)
    , ("blurry-spheres",  return blurrySpheres)
    , ("cube",            loadCubeScene)
    , ("bunny",           loadBunnyScene)
    , ("table",           loadTableScene)
    , ("dragon",          loadDragonScene)
    , ("monkey",          loadMonkeyScene)
    , ("rectangles",      return rectangles)
    , ("area-light",      return areaLightScene)
    , ("refl",            return reflScene)
    ]
