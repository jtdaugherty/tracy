{-# LANGUAGE TemplateHaskell #-}
module Tracy.World where

import Control.Applicative
import Control.Lens
import Control.Monad.State
import Data.List
import Data.Maybe
import Data.Ord (comparing)
import Linear
import Codec.BMP
import qualified Data.ByteString as B

import Tracy.Types
import Tracy.Util

data World =
    World { _viewPlane :: ViewPlane
          , _bgColor :: Color
          , _objects :: [Object]
          }

data TraceState =
    TraceState { _traceLog :: [String]
               }
    deriving (Show)

makeLenses ''World
makeLenses ''TraceState

type TraceM a = State TraceState a

logMsg :: String -> TraceM ()
logMsg msg =
    modify (traceLog %~ (msg:))

renderWorld :: World -> TraceM BMP
renderWorld w = do
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
          in case hitAnObject w ray of
               Nothing -> w^.bgColor
               Just (sh, _t) -> sh^.shadeColor

  logMsg "Tracing."
  return $ packRGBA32ToBMP (fromEnum $ w^.viewPlane^.hres)
             (fromEnum $ w^.viewPlane^.vres) bytes

hitAnObject :: World -> Ray -> Maybe (Shade, Double)
hitAnObject w r =
    listToMaybe $ sortBy (comparing snd) $ catMaybes results
    where
      results = tests <*> pure r
      tests = (w^.objects) ^.. (folded . hit)
