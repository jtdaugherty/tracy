module Tracy.Scenes where

import Control.Lens
import Data.Colour
import Linear

import Tracy.Types
import Tracy.Sphere
import Tracy.Plane
import Tracy.Cameras

defaultVp :: ViewPlane
defaultVp =
    ViewPlane { _hres = 400
              , _vres = 400
              , _pixelSize = 1.0
              , _gamma = 1.0
              , _inverseGamma = 1.0
              }

camera :: Camera Pinhole
camera =
    pinholeCamera (V3 0 0 500)
                  (V3 0 0 0)
                  (V3 0 1 0)
                  500
                  1.0
                  1.0

vpd :: Double -> Camera Pinhole -> Camera Pinhole
vpd d cam = cam & (cameraData.pinholeVPDistance) .~ d

eye :: V3 Double -> Camera Pinhole -> Camera Pinhole
eye e cam = cam & (cameraData.pinholeEyePoint) .~ e

world :: [Object] -> World
world os = World { _viewPlane = defaultVp
                 , _objects = os
                 , _bgColor = cBlack
                 }

world1 :: (Camera Pinhole, World)
world1 =
    let s = sphere (V3 0 0 0) 85.0 cRed
    in ( camera, world [s] )

world2 :: (Camera Pinhole, World)
world2 =
    let s = sphere (V3 0 0 11) 30.0 cBlue
        s2 = sphere (V3 0 0 0) 40.0 cGreen
    in ( camera, world [s, s2] )

world3 :: (Camera Pinhole, World)
world3 =
    let s = sphere (V3 0 0 11) 30.0 cBlue
        p = plane (V3 0 0 0) (V3 0 1 0.1) cGreen
    in ( camera, world [s, p] )

world4 :: (Camera Pinhole, World)
world4 =
    let s = sphere (V3 0 0 11) 30.0 cBlue
        p = plane (V3 0 0 0) (V3 0 1 0.1) cGreen
        s2 = sphere (V3 50 5 0) 10.0 cMagenta
        s3 = sphere (V3 (-50) 10 0) 15.0 cYellow
    in ( camera, world [s, p, s2, s3] )

scenes :: [(FilePath, (Camera Pinhole, World))]
scenes =
    [ ("world1.bmp", world1)
    , ("world2.bmp", world2)
    , ("world3.bmp", world3)
    , ("world4.bmp", world4)
    ]
