module Tracy.Scenes where

import Linear
import Data.Colour

import Tracy.Types
import Tracy.Sphere
import Tracy.Plane

defaultVp :: ViewPlane
defaultVp =
    ViewPlane { _hres = 400
              , _vres = 400
              , _pixelSize = 1.0
              , _gamma = 1.0
              , _inverseGamma = 1.0
              }

world1 :: World
world1 =
    let s = sphere (V3 0 0 0) 85.0 cRed
    in World { _viewPlane = defaultVp
             , _objects = [s]
             , _bgColor = cBlack
             , _viewPlaneDistance = 50
             , _eyePoint = V3 0 0 100
             }

world2 :: World
world2 =
    let s = sphere (V3 0 0 11) 30.0 cBlue
        s2 = sphere (V3 0 0 0) 40.0 cGreen
    in World { _viewPlane = defaultVp
             , _objects = [s, s2]
             , _bgColor = cBlack
             , _viewPlaneDistance = 200
             , _eyePoint = V3 0 0 100
             }

world3 :: World
world3 =
    let s = sphere (V3 0 0 11) 30.0 cBlue
        p = plane (V3 0 0 0) (V3 0 1 0.1) cGreen
    in World { _viewPlane = defaultVp
             , _objects = [s, p]
             , _bgColor = cBlack
             , _viewPlaneDistance = 200
             , _eyePoint = V3 0 0 100
             }

world4 :: World
world4 =
    let s = sphere (V3 0 0 11) 30.0 cBlue
        p = plane (V3 0 0 0) (V3 0 1 0.1) cGreen
        s2 = sphere (V3 50 5 0) 10.0 cMagenta
        s3 = sphere (V3 (-50) 10 0) 15.0 cYellow
    in World { _viewPlane = defaultVp
             , _objects = [s, p, s2, s3]
             , _bgColor = cBlack
             , _viewPlaneDistance = 200
             , _eyePoint = V3 0 0 100
             }

scenes :: [(FilePath, World)]
scenes =
    [ ("world1.bmp", world1)
    , ("world2.bmp", world2)
    , ("world3.bmp", world3)
    , ("world4.bmp", world4)
    ]
