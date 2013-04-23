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

world :: [Object] -> World
world os = World { _viewPlane = defaultVp
                 , _objects = os
                 , _bgColor = cBlack
                 }

world1 :: (Camera Pinhole, World)
world1 =
    let s = sphere (V3 0 0 0) 85.0 cRed
    in ( defCamera, world [s] )

world2 :: (Camera Pinhole, World)
world2 =
    let s = sphere (V3 0 0 11) 30.0 cBlue
        s2 = sphere (V3 0 0 0) 40.0 cGreen
    in ( defCamera, world [s, s2] )

world3 :: (Camera Pinhole, World)
world3 =
    let s = sphere (V3 0 0 11) 30.0 cBlue
        p = plane (V3 0 0 0) (V3 0 1 0.1) cGreen
    in ( defCamera, world [s, p] )

world4 :: (Camera Pinhole, World)
world4 =
    let s = sphere (V3 0 0 11) 30.0 cBlue
        p = plane (V3 0 0 0) (V3 0 1 0.1) cGreen
        s2 = sphere (V3 50 5 0) 10.0 cMagenta
        s3 = sphere (V3 (-50) 10 0) 15.0 cYellow
    in ( defCamera, world [s, p, s2, s3] )

world5 :: (Camera ThinLens, World)
world5 =
    let spheres = concat [ ss y | y <- [-200, -100, 0, 100, 200] ]
        pairs = [ (-300, cWhite)
                , (-250, cYellow)
                , (-200, cCyan)
                , (-150, cRed)
                , (-100, cMagenta)
                , (-50, cBlue)
                , (0, cGreen)
                , (50, cWhite)
                , (100, cYellow)
                ]
        ss y = [ sphere (V3 xz y xz) 30.0 c
                 | (xz, c) <- pairs
               ]
    in ( lensCam, world spheres )

scenes :: [(String, (World, TraceM BMP))]
scenes =
    let render (c, w) = (w, (c^.cameraRenderWorld) c w)
    in [ ("world1", render world1)
       , ("world2", render world2)
       , ("world3", render world3)
       , ("world4", render world4)
       , ("world5", render world5)
       ]
