module Main where

import Control.Lens
import Control.Monad
import Data.Colour
import Data.Time.Clock
import Linear
import Codec.BMP

import Tracy.Types
import Tracy.Sphere
import Tracy.Plane
import Tracy.World

defaultVp :: ViewPlane
defaultVp =
    ViewPlane { _hres = 200
              , _vres = 200
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
             }

world2 :: World
world2 =
    let s = sphere (V3 0 0 11) 30.0 cBlue
        s2 = sphere (V3 0 0 0) 40.0 cGreen
    in World { _viewPlane = defaultVp
             , _objects = [s, s2]
             , _bgColor = cBlack
             }

world3 :: World
world3 =
    let s = sphere (V3 0 0 11) 30.0 cBlue
        p = plane (V3 0 0 0) (V3 0 1 1) cGreen
    in World { _viewPlane = defaultVp
             , _objects = [s, p]
             , _bgColor = cBlack
             }

scenes :: [(FilePath, World)]
scenes =
    [ ("world1.bmp", world1)
    , ("world2.bmp", world2)
    , ("world3.bmp", world3)
    ]

main :: IO ()
main = do
  forM_ scenes $ \(filename, w) ->
      do
        putStrLn $ "Rendering " ++ filename ++ " ..."
        putStrLn $ "  Objects: " ++ (w^.objects^.to length^.to show)
        t1 <- getCurrentTime
        writeBMP filename $ renderScene w
        t2 <- getCurrentTime
        putStrLn $ "done. Total time: " ++ (show $ diffUTCTime t2 t1)
