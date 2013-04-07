{-# LANGUAGE TemplateHaskell #-}
module Tracy.World where

import Control.Applicative
import Control.Lens
import Linear
import Codec.BMP
import qualified Data.ByteString as B

import Tracy.Types
import Tracy.Util

data World =
    World { _viewPlane :: ViewPlane
          , _bgColor :: Color
          , _worldSphere :: Object
          }

makeLenses ''World

renderScene :: World -> BMP
renderScene w =
    let zw = 100
        rayDir = V3 0 0 (-1)

        vp = w^.viewPlane
        bytes = B.concat $ getColorBytes <$> colors
        colors = concat getRows
        getRows = getRow <$> [0..vp^.vres-1]
        getRow r = getCol r <$> [0..vp^.hres-1]
        getCol row col =
            let x = vp^.pixelSize * (col - 0.5 * (vp^.hres - 1))
                y = vp^.pixelSize * (row - 0.5 * (vp^.vres - 1))
                ray = Ray { _origin = V3 x y zw
                          , _direction = rayDir
                          }
            in case (w^.worldSphere^.hit) ray of
                 Nothing -> w^.bgColor
                 Just (sh, _t) -> sh^.shadeColor

    in packRGBA32ToBMP (fromEnum $ w^.viewPlane^.hres)
           (fromEnum $ w^.viewPlane^.vres) bytes
