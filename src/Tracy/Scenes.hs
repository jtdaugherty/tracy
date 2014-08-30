{-# LANGUAGE TemplateHaskell #-}
module Tracy.Scenes where

import Control.Lens
import Data.Colour
import Linear

import Tracy.Types

defaultVp :: ViewPlane
defaultVp =
    ViewPlane { _hres = 800
              , _vres = 800
              , _pixelSize = 0.5
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
                   300
                   300
                   0

world :: [ObjectDesc] -> [LightDesc] -> WorldDesc
world os ls = WorldDesc { _wdViewPlane = defaultVp
                        , _wdObjects = os
                        , _wdBgColor = cBlack
                        , _wdLights = Point True 2 cWhite (V3 (-500) 500 500) : ls
                        , _wdAmbient = Ambient 1 cWhite
                        , _wdWorldShadows = True
                        }

worldOcc :: [ObjectDesc] -> [LightDesc] -> WorldDesc
worldOcc os ls = WorldDesc { _wdViewPlane = defaultVp
                           , _wdObjects = os
                           , _wdBgColor = cBlack
                           , _wdLights = Point True 2 cWhite (V3 (-500) 500 500) : ls
                           , _wdAmbient = AmbientOccluder cWhite (grey 0.25) 1
                           , _wdWorldShadows = True
                           }

world1 :: SceneDesc
world1 =
    let s = Sphere (V3 (-40) 0 0) 85.0 (Phong cRed 100)
    in SceneDesc (world [s] []) NoScheme defCamera

world2 :: SceneDesc
world2 =
    let s = Sphere (V3 0 0 41) 10.0 (Matte cBlue)
        s2 = Sphere (V3 0 0 0) 40.0 (Matte cGreen)
    in SceneDesc (world [s, s2] []) NoScheme defCamera

world2occ :: SceneDesc
world2occ =
    let s = Sphere (V3 0 0 41) 10.0 (Matte cBlue)
        s2 = Sphere (V3 0 0 0) 40.0 (Matte cGreen)
    in SceneDesc (worldOcc [s, s2] []) NoScheme defCamera

world3 :: SceneDesc
world3 =
    let s = Sphere (V3 0 60 80) 30.0 (Matte cBlue)
        p = Plane (V3 0 0 0) (V3 0 1 0) (Matte cGreen)
    in SceneDesc (world [s, p] []) NoScheme defCamera

world3occ :: SceneDesc
world3occ =
    let s = Sphere (V3 0 60 80) 30.0 (Matte cBlue)
        p = Plane (V3 0 0 0) (V3 0 1 0) (Matte cGreen)
    in SceneDesc (worldOcc [s, p] []) NoScheme defCamera

world4 :: SceneDesc
world4 =
    let s = Sphere (V3 0 0 11) 30.0 (Matte cBlue)
        p = Plane (V3 0 0 0) (V3 0 1 0) (Matte cGreen)
        s2 = Sphere (V3 50 5 0) 10.0 (Matte cMagenta)
        s3 = Sphere (V3 (-50) 10 0) 15.0 (Matte cYellow)
        b1 = Box (V3 100 0 0) (V3 150 50 50) (Matte cCyan)
        b2 = Box (V3 100 0 (-100)) (V3 150 50 (-50)) (Matte cRed)
        b3 = Box (V3 (-150) 0 25) (V3 (-100) 75 75) (Matte cYellow)
        b4 = Box (V3 (-150) 0 (-75)) (V3 (-100) 75 (-25)) (Matte cWhite)
        t1 = Triangle (V3 100 50 0) (V3 50 100 0) (V3 (-50) 75 0) (Matte cWhite)
    in SceneDesc (world [t1, s, p, s2, s3, b1, b2, b3, b4] []) GridScheme defCamera

world4occ :: SceneDesc
world4occ =
    let s = Sphere (V3 0 0 11) 30.0 (Matte cBlue)
        p = Plane (V3 0 0 0) (V3 0 1 0) (Matte cGreen)
        s2 = Sphere (V3 50 5 0) 10.0 (Matte cMagenta)
        s3 = Sphere (V3 (-50) 10 0) 15.0 (Matte cYellow)
        b1 = Box (V3 100 0 0) (V3 150 50 50) (Matte cCyan)
        b2 = Box (V3 100 0 (-100)) (V3 150 50 (-50)) (Matte cRed)
        b3 = Box (V3 (-150) 0 25) (V3 (-100) 75 75) (Matte cYellow)
        b4 = Box (V3 (-150) 0 (-75)) (V3 (-100) 75 (-25)) (Matte cWhite)
        t1 = Triangle (V3 100 50 0) (V3 50 100 0) (V3 (-50) 75 0) (Matte cWhite)
    in SceneDesc (worldOcc [t1, s, p, s2, s3, b1, b2, b3, b4] []) NoScheme defCamera

world5 :: SceneDesc
world5 =
    let spheres = concat [ ss y e | (y, e) <- params ]
        params = [ (-500, 1)
                 , (-400, 10)
                 , (-300, 10)
                 , (-200, 20)
                 , (-100, 75)
                 , (0, 200)
                 , (100, 75)
                 , (200, 20)
                 , (300, 10)
                 , (400, 10)
                 , (500, 1)
                 ]
        pairs = [ (-500, (Phong cRed))
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
                ]
        ss y e = [ Sphere (V3 xz y xz) 30.0 (mkMat e)
                   | (xz, mkMat) <- pairs
                 ]
    in SceneDesc (world spheres [] & wdWorldShadows .~ False)
                 GridScheme
                 (defCamera & thinLensRadius .~ 10.0)

world6 :: SceneDesc
world6 =
    let s = Sphere (V3 0 60 0) 30.0 (Phong cBlue 100)
        p = Plane (V3 0 0 0) (V3 0 1 0) (Matte cGreen)
    in SceneDesc (world [s, p] []) NoScheme defCamera

world6occ :: SceneDesc
world6occ =
    let s = Sphere (V3 0 30 0) 30.0 (Phong cBlue 100)
        p = Plane (V3 0 0 0) (V3 0 1 0) (Matte cGreen)
    in SceneDesc (worldOcc [s, p] []) NoScheme defCamera

scenes :: [(String, SceneDesc)]
scenes =
    [ ("world1", world1)
    , ("world2", world2)
    , ("world2occ", world2occ)
    , ("world3", world3)
    , ("world3occ", world3occ)
    , ("world4", world4)
    , ("world4occ", world4occ)
    , ("world5", world5)
    , ("world6", world6)
    , ("world6occ", world6occ)
    ]
