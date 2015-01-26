{-# LANGUAGE TemplateHaskell #-}
module Tracy.Scenes
  ( allScenes
  ) where

import Control.Lens
import Data.Colour
import Linear

import Tracy.Types

defaultVp :: ViewPlane
defaultVp =
    ViewPlane { _hres = 800
              , _vres = 800
              , _pixelSize = 1.0
              , _gamma = 1.0
              , _inverseGamma = 1.0
              , _maxDepth = 10
              }

defCamera :: CameraDesc
defCamera =
    ThinLensCamera (V3Val $ V3 0 100 300)
                   (V3 0 0 0)
                   (V3 0 1 0)
                   1.0
                   1.0
                   500 -- vpDist
                   300 -- fpDist
                   (DoubleVal 0)
                   (UnitDisk CorrelatedMultiJittered)

world :: [ObjectDesc] -> [LightDesc] -> WorldDesc
world os ls = WorldDesc { _wdViewPlane = defaultVp
                        , _wdObjects = os
                        , _wdBgColor = cBlack
                        , _wdLights = ls
                        , _wdAmbient = Ambient 1 cWhite
                        , _wdWorldShadows = True
                        }

worldOcc :: [ObjectDesc] -> [LightDesc] -> Double -> WorldDesc
worldOcc os ls ambStr = WorldDesc { _wdViewPlane = defaultVp
                                  , _wdObjects = os
                                  , _wdBgColor = cBlack
                                  , _wdLights =  ls
                                  , _wdAmbient = AmbientOccluder cWhite cBlack ambStr
                                  , _wdWorldShadows = True
                                  }

bunnyScene :: SceneDesc
bunnyScene =
    let cObj = Mesh (MeshFile "meshes/bunny.ply") $ Phong cWhite 0.5 100
        p = Plane (V3 0 0 0.1) (V3 0 0 1) (Matte cGreen)
        ls = [ Point True 1 cWhite (V3 (-5) 5 5)
             , Point True 1 cWhite (V3 5 5 5)
             , Point True 1 cWhite (V3 5 (-5) 5)
             , Point True 1 cWhite (V3 5 (-5) (-5))
             , Point True 1 cWhite (V3 5 0 2)
             ]
    in SceneDesc (worldOcc [cObj, p] ls 1) NoScheme
                 (defCamera & thinLensRadius .~ (DoubleVal 0.0)
                            & thinLensLookAt .~ (V3 0 0 2)
                            & thinLensVpDist .~ 300
                            & thinLensFpDist .~ 300
                            & thinLensUp     .~ (V3 0 0 1)
                            & thinLensEye    .~ (V3Val $ V3 (-6) (-3) 3)
                            )
                 RayCastTracer

tableScene :: SceneDesc
tableScene =
    let tableObj =  Mesh (MeshFile "meshes/table.ply") $ Phong (Colour (100.0/255.0) (73.0/255.0) 0) 0.5 100
        monkeyObj = Mesh (MeshFile "meshes/monkey2.ply") $ GlossyReflective cCyan 0.7 10 cWhite 0.7 100
        torusObj =  Mesh (MeshFile "meshes/torus.ply") $ Reflective (Colour 1 0.7 0.7) 0.9 1000 (Colour 1 0.7 0.7) 0.7
        icoObj =   Mesh (MeshFile "meshes/ico1.ply") $ Matte cYellow
        sphereObj = Sphere (V3 0.10225 4.449909 (-1.18322)) 0.811 $ Reflective (grey 0.3) 0.9 1000 cWhite 0.9
        chairLeft = Mesh (MeshFile "meshes/chairLeft.ply") $ Phong (Colour 1 (79/255) 0) 1.0 100000
        chairRight = Mesh (MeshFile "meshes/chairRight.ply") $ Phong (Colour 1 (79/255) 0) 1.0 100000

        a = Rectangle (V3 (-2.5) 8 0) (V3 5 0 0) (V3 0 0 5) (Emissive cWhite 6)

        p_bottom = Mesh (MeshFile "meshes/plane_bottom.ply") $ Matte cWhite
        p_top = Mesh (MeshFile "meshes/plane_top.ply") $ Matte cWhite
        p_left = Mesh (MeshFile "meshes/plane_left.ply") $ Matte (Colour (67/255) (151/255) (224/255))
        p_right = Mesh (MeshFile "meshes/plane_right.ply") $ Matte (Colour (67/255) (151/255) (224/255))
        p_back = Mesh (MeshFile "meshes/plane_back.ply") $ Matte cWhite
        p_front = Mesh (MeshFile "meshes/plane_front.ply") $ Matte cWhite

    in SceneDesc (world [ chairLeft, chairRight, tableObj
                        , monkeyObj, torusObj, icoObj
                        , sphereObj, a
                        , p_bottom, p_back, p_left, p_right, p_top, p_front
                        ] []) NoScheme
                 (defCamera & thinLensRadius .~ (DoubleVal 0.0)
                            & thinLensLookAt .~ (V3 0 3 0)
                            & thinLensVpDist .~ 500
                            & thinLensFpDist .~ 500
                            & thinLensEye .~ (V3Val $ V3 0 5 3.5)
                            )
                 PathTracer

dragonScene :: SceneDesc
dragonScene =
    let cObj = Mesh (MeshFile "meshes/dragon.ply") $ Phong cRed 0.5 50
        p = Plane (V3 0 0.002 0) (V3 0 1 0) (Matte cWhite)
        r = Rectangle (V3 0 1 0) (V3 0.1 0 0) (V3 0 0 0.1) (Emissive cWhite 200)
        ls = [ Area True r Nothing
             ]
    in SceneDesc (worldOcc [cObj, p] ls 1) NoScheme
                 (defCamera & thinLensRadius .~ (DoubleVal 0.0)
                            & thinLensLookAt .~ (V3 0 0.15 0)
                            & thinLensVpDist .~ 300
                            & thinLensFpDist .~ 300
                            & thinLensEye    .~ (V3Val $ V3 (-0.1) 0.1 0.2)
                            )
                 AreaLightTracer

monkeyScene :: SceneDesc
monkeyScene =
    let cObj = Mesh (MeshFile "meshes/monkey.ply") $ Phong cWhite 0.5 50
        ls = [ Point True 1 cWhite (V3 (-10) 10 10)
             ]
    in SceneDesc (worldOcc [cObj] ls 1) NoScheme
                 (defCamera & thinLensRadius .~ (DoubleVal 0.0)
                            & thinLensLookAt .~ (V3 0 0 0)
                            & thinLensVpDist .~ 300
                            & thinLensFpDist .~ 300
                            & thinLensEye    .~ (V3Val $ V3 0 0 4)
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

        i1 = Instances a [ ID [Translate (-100) 30 (-100), RotateX (pi/6)] (Just $ Emissive cRed 3)]
        i2 = Instances a [ ID [Translate (-10) 30 (-100), RotateX (pi/4)]  (Just $ Emissive cGreen 3)]
        i3 = Instances a [ ID [Translate 110 30 (-100), RotateX (pi/3)]    (Just $ Emissive cBlue 3)]

        ls = [ Area True i1 Nothing
             , Area True i2 Nothing
             , Area True i3 Nothing
             ]
    in SceneDesc (worldOcc [Grid [i1, i2, i3], p] ls 1) NoScheme defCamera AreaLightTracer

reflScene :: SceneDesc
reflScene =
    let s = Sphere (V3 0 40 0) 40 (GlossyReflective cWhite 0 50 cWhite 0.9 50)
        p = Plane (V3 0 0 0) (V3 0 1 0) (GlossyReflective cWhite 0.5 10 cWhite 1 100)

        ss = Instances s [ ID [Translate (-90) 0 0]     (Just $ GlossyReflective cBlue    0.5 10 cWhite 0.8 10)
                         , ID [Translate 0 0 0]         (Just $ GlossyReflective cWhite   0.8 50 cWhite 0.9 500)
                         , ID [Translate 90 0 0]        (Just $ GlossyReflective cYellow  1 500 cWhite 0.8 100000)
                         , ID [Translate (-90) 0 90]    (Just $ GlossyReflective cRed     0.5 10 cWhite 0.8 10)
                         , ID [Translate 0 0 90]        (Just $ GlossyReflective cGreen   0.8 50 cWhite 0.9 500)
                         , ID [Translate 90 0 90]       (Just $ GlossyReflective cMagenta 1 500 cWhite 0.8 100000)
                         , ID [Translate (-90) 0 (-90)] (Just $ GlossyReflective cYellow  0.5 10 cWhite 0.9 10)
                         , ID [Translate 0 0 (-90)]     (Just $ GlossyReflective cBlack   0.8 50 cWhite 0.9 500)
                         , ID [Translate 90 0 (-90)]    (Just $ GlossyReflective cCyan    1 500 cWhite 0.9 100000)
                         ]
        r1 = Rectangle (V3 (-100) 300 0) (V3 200 0 0) (V3 0.0 30.901697 95.10565) (Emissive cWhite 3)
        ls = [ Area True r1 $ Just 200000
             ]
    in SceneDesc (worldOcc [Grid [ss, r1], p] ls 1) NoScheme
                 (defCamera & thinLensEye .~ (V3LerpRotY (1, 200) (V3 200 250 250) (2 * pi))
                 )
                 AreaLightTracer

allScenes :: [(String, SceneDesc)]
allScenes =
    [ ("bunny",           bunnyScene)
    , ("table",           tableScene)
    , ("dragon",          dragonScene)
    , ("monkey",          monkeyScene)
    , ("rectangles",      rectangles)
    , ("area-light",      areaLightScene)
    , ("refl",            reflScene)
    ]
