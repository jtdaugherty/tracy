{-# LANGUAGE TemplateHaskell #-}
module Tracy.Scenes where

import Control.Lens
import Data.Colour
import Linear

import Tracy.Types
import Tracy.Sphere
import Tracy.Triangle
import Tracy.Box
import Tracy.Plane
import Tracy.Cameras
import Tracy.Samplers
import Tracy.Lights
import Tracy.BRDF
import Tracy.Materials
import Tracy.AccelSchemes

data Scene a =
    Scene { _sceneWorld :: World
          , _sceneAccelScheme :: AccelScheme
          , _sceneCamera :: Camera a
          }

makeLenses ''Scene

defaultVp :: ViewPlane
defaultVp =
    ViewPlane { _hres = 400
              , _vres = 400
              , _pixelSize = 1.0
              , _gamma = 1.0
              , _inverseGamma = 1.0
              }

defCamera :: Camera ThinLens
defCamera =
    thinLensCamera (V3 0 100 300)
                       (V3 0 0 0)
                       (V3 0 1 0)
                       1.0
                       1.0
                       300
                       300
                       0
                       (toUnitDisk jittered)

mat :: Color -> Material
mat c = matte
        (lambertian (toUnitHemisphere jittered) c 0.25)
        (lambertian (toUnitHemisphere jittered) c 0.65)

ph :: Color -> Float -> Material
ph c e = phong
         (lambertian (toUnitHemisphere jittered) c 0.25)
         (lambertian (toUnitHemisphere jittered) c 0.65)
         (glossySpecular c e)

world :: [Object] -> [Light] -> World
world os ls = World { _viewPlane = defaultVp
                    , _objects = os
                    , _bgColor = cBlack
                    , _lights = pointLight True 2 cWhite (V3 (-500) 500 500) :
                                ls
                    , _ambient = ambientLight 1 cWhite
                    , _worldShadows = True
                    }

world1 :: Scene ThinLens
world1 =
    let s = sphere (V3 (-40) 0 0) 85.0 (ph cRed 100)
    in Scene (world [s] []) noScheme defCamera

world2 :: Scene ThinLens
world2 =
    let s = sphere (V3 0 0 41) 10.0 (mat cBlue)
        s2 = sphere (V3 0 0 0) 40.0 (mat cGreen)
    in Scene (world [s, s2] []) noScheme defCamera

world3 :: Scene ThinLens
world3 =
    let s = sphere (V3 0 60 80) 30.0 (mat cBlue)
        p = plane (V3 0 0 0) (V3 0 1 0) (mat cGreen)
    in Scene (world [s, p] []) noScheme defCamera

world4 :: Scene ThinLens
world4 =
    let s = sphere (V3 0 0 11) 30.0 (mat cBlue)
        p = plane (V3 0 0 0) (V3 0 1 0) (mat cGreen)
        s2 = sphere (V3 50 5 0) 10.0 (mat cMagenta)
        s3 = sphere (V3 (-50) 10 0) 15.0 (mat cYellow)
        b1 = box (V3 100 0 0) (V3 150 50 50) (mat cCyan)
        b2 = box (V3 100 0 (-100)) (V3 150 50 (-50)) (mat cRed)
        b3 = box (V3 (-150) 0 25) (V3 (-100) 75 75) (mat cYellow)
        b4 = box (V3 (-150) 0 (-75)) (V3 (-100) 75 (-25)) (mat cWhite)
        t1 = tri (V3 100 50 0) (V3 50 100 0) (V3 (-50) 75 0) (mat cWhite)
    in Scene (world [t1, s, p, s2, s3, b1, b2, b3, b4] []) gridScheme defCamera

world5 :: Scene ThinLens
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
        pairs = [ (-500, (ph cRed))
                , (-450, (ph cMagenta))
                , (-400, (ph cBlue))
                , (-350, (ph cGreen))
                , (-300, (ph cWhite))
                , (-250, (ph cYellow))
                , (-200, (ph cCyan))
                , (-150, (ph cRed))
                , (-100, (ph cMagenta))
                , (-50, (ph cBlue))
                , (0, (ph cGreen))
                , (50, (ph cWhite))
                , (100, (ph cYellow))
                ]
        ss y e = [ sphere (V3 xz y xz) 30.0 (mkMat e)
                   | (xz, mkMat) <- pairs
                 ]
    in Scene (world spheres [] & worldShadows .~ False)
             gridScheme
             (defCamera & cameraData.lensRadius .~ 25.0)

world6 :: Scene ThinLens
world6 =
    let s = sphere (V3 0 60 0) 30.0 (ph cBlue 100)
        p = plane (V3 0 0 0) (V3 0 1 0) (mat cGreen)
    in Scene (world [s, p] []) noScheme defCamera

scenes :: [(String, Scene ThinLens)]
scenes =
    [ ("world1", world1)
    , ("world2", world2)
    , ("world3", world3)
    , ("world4", world4)
    , ("world5", world5)
    , ("world6", world6)
    ]
