module Main where

import Control.Monad
import Data.Colour
import Linear
import Codec.BMP

import Tracy.Types
import Tracy.Sphere
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
             , _worldSphere = s
             , _bgColor = cBlack
             }

world2 :: World
world2 =
    let s = sphere (V3 0 0 0) 30.0 cBlue
    in World { _viewPlane = defaultVp
             , _worldSphere = s
             , _bgColor = cBlack
             }

scenes :: [(FilePath, World)]
scenes =
    [ ("world1.bmp", world1)
    , ("world2.bmp", world2)
    ]

main :: IO ()
main = do
  forM_ scenes $ \(filename, w) ->
      do
        putStr $ "Rendering " ++ filename ++ "..."
        writeBMP filename $ renderScene w
        putStrLn "done."
