module Tracy.Scenes where

import Control.Lens
import Data.Colour
import Linear
import Codec.BMP

import Tracy.Types
import Tracy.Sphere
import Tracy.Plane
import Tracy.Cameras
import Tracy.Samplers
import Tracy.Lights
import Tracy.BRDF
import Tracy.Materials

defaultVp :: ViewPlane
defaultVp =
    ViewPlane { _hres = 400
              , _vres = 400
              , _pixelSize = 1.0
              , _gamma = 1.0
              , _inverseGamma = 1.0
              }

defCamera :: Camera Pinhole
defCamera =
    pinholeCamera (V3 0 0 500)
                  (V3 0 0 0)
                  (V3 0 1 0)
                  1.0
                  1.0
                  500

lensCam :: Camera ThinLens
lensCam =
    thinLensCamera (V3 0 0 300)
                       (V3 0 0 0)
                       (V3 0 1 0)
                       1.0
                       1.0
                       300
                       300
                       25.0
                       (toUnitDisk jittered)
mat :: Color -> Material
mat c = matte
        (lambertian (toUnitHemisphere jittered) c 0.25)
        (lambertian (toUnitHemisphere jittered) c 0.65)

world :: [Object] -> [Light] -> World
world os ls = World { _viewPlane = defaultVp
                    , _objects = os
                    , _bgColor = cBlack
                    , _lights = pointLight 3.0 cWhite (V3 (-200) 200 400) : ls
                    , _ambient = ambientLight 1.0 cWhite
                    }

world1 :: (Camera Pinhole, World)
world1 =
    let s = sphere (V3 0 0 0) 85.0 (mat cRed)
    in ( defCamera, world [s] [] )

world2 :: (Camera Pinhole, World)
world2 =
    let s = sphere (V3 0 0 11) 30.0 (mat cBlue)
        s2 = sphere (V3 0 0 0) 40.0 (mat cGreen)
    in ( defCamera, world [s, s2] [] )

world3 :: (Camera Pinhole, World)
world3 =
    let s = sphere (V3 0 0 11) 30.0 (mat cBlue)
        p = plane (V3 0 0 0) (V3 0 1 0.1) (mat cGreen)
    in ( defCamera, world [s, p] [] )

world4 :: (Camera Pinhole, World)
world4 =
    let s = sphere (V3 0 0 11) 30.0 (mat cBlue)
        p = plane (V3 0 0 0) (V3 0 1 0.1) (mat cGreen)
        s2 = sphere (V3 50 5 0) 10.0 (mat cMagenta)
        s3 = sphere (V3 (-50) 10 0) 15.0 (mat cYellow)
    in ( defCamera, world [s, p, s2, s3] [] )

world5 :: (Camera ThinLens, World)
world5 =
    let spheres = concat [ ss y | y <- [-500, -400, -300, -200, -100, 0, 100, 200, 300, 400, 500] ]
        pairs = [ (-500, (mat cRed))
                , (-450, (mat cMagenta))
                , (-400, (mat cBlue))
                , (-350, (mat cGreen))
                , (-300, (mat cWhite))
                , (-250, (mat cYellow))
                , (-200, (mat cCyan))
                , (-150, (mat cRed))
                , (-100, (mat cMagenta))
                , (-50, (mat cBlue))
                , (0, (mat cGreen))
                , (50, (mat cWhite))
                , (100, (mat cYellow))
                ]
        ss y = [ sphere (V3 xz y xz) 30.0 c
                 | (xz, c) <- pairs
               ]
    in ( lensCam, world spheres [] )

scenes :: [(String, (World, TraceM BMP))]
scenes =
    let render (c, w) = (w, (c^.cameraRenderWorld) c w)
    in [ ("world1", render world1)
       , ("world2", render world2)
       , ("world3", render world3)
       , ("world4", render world4)
       , ("world5", render world5)
       ]
