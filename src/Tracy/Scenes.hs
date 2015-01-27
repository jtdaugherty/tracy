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

allScenes :: [(String, SceneDesc)]
allScenes =
    [ ("rectangles",      rectangles)
    , ("area-light",      areaLightScene)
    ]
