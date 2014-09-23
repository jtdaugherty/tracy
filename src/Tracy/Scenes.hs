{-# LANGUAGE TemplateHaskell #-}
module Tracy.Scenes
  ( allScenes
  ) where

import Control.Lens
import Data.Colour
import Linear

import Tracy.Types
import Tracy.Objects.Mesh

defaultVp :: ViewPlane
defaultVp =
    ViewPlane { _hres = 800
              , _vres = 800
              , _pixelSize = 1.0
              , _gamma = 1.0
              , _inverseGamma = 1.0
              }

defCamera :: CameraDesc
defCamera =
    ThinLensCamera (V3 0 100 300)
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
    let s = Sphere (V3 (-40) 0 0) 85.0 (Phong cRed 100)
    in SceneDesc (world [s] []) NoScheme defCamera

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
    in SceneDesc (worldOcc [t1, s, p, s2, s3, b1, b2, b3, b4] ls 3) NoScheme defCamera

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
    in SceneDesc (worldOcc [t1, s, p, s2, s3, b1, b2, b3, b4] [] 4) NoScheme defCamera

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
        ss y e r = [ Sphere (V3 xz y xz) r (mkMat e)
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
                            & thinLensEye    .~ (V3 0 50 300))

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
                            & thinLensEye    .~ (V3 2.1 2.1 3)
                            )

loadBunnyScene :: IO SceneDesc
loadBunnyScene = do
    mDesc <- loadMesh "meshes/bunny.ply"
    let cObj = Mesh mDesc $ Phong cWhite 100
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
                            & thinLensEye    .~ (V3 (-6) (-3) 3)
                            )

loadDragonScene :: IO SceneDesc
loadDragonScene = do
    mDesc <- loadMesh "meshes/dragon.ply"
    let cObj = Mesh mDesc $ Phong cRed 50
        p = Plane (V3 0 0.002 0) (V3 0 1 0) (Matte cWhite)
        ls = [ Point True 1 cWhite (V3 0 2 0)
             ]
    return $ SceneDesc (worldOcc [cObj, p] ls 1) NoScheme
                 (defCamera & thinLensRadius .~ 0.0
                            & thinLensLookAt .~ (V3 0 0.15 0)
                            & thinLensVpDist .~ 300
                            & thinLensFpDist .~ 300
                            & thinLensEye    .~ (V3 (-0.1) 0.1 0.2)
                            )

loadMonkeyScene :: IO SceneDesc
loadMonkeyScene = do
    mDesc <- loadMesh "meshes/monkey.ply"
    let cObj = Mesh mDesc $ Phong cMagenta 50
        p = Plane (V3 0 0 0) (V3 0 1 0) (Matte cWhite)
        ls = [ Point True 1 cWhite (V3 0 20 0)
             ]
    return $ SceneDesc (worldOcc [cObj, p] ls 1) NoScheme
                 (defCamera & thinLensRadius .~ 0.0
                            & thinLensLookAt .~ (V3 0 2 0)
                            & thinLensVpDist .~ 300
                            & thinLensFpDist .~ 300
                            & thinLensEye    .~ (V3 (-3) 2.5 4)
                            )

allScenes :: [(String, IO SceneDesc)]
allScenes =
    [ ("one-sphere",      return oneSphere)
    , ("object-demo",     return objectDemo)
    , ("object-demo2",    return objectDemoNoPoint)
    , ("clear-spheres",   return clearSpheres)
    , ("blurry-spheres",  return blurrySpheres)
    , ("cube",            loadCubeScene)
    , ("bunny",           loadBunnyScene)
    , ("dragon",          loadDragonScene)
    , ("monkey",          loadMonkeyScene)
    ]
