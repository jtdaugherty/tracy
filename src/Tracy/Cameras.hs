{-# LANGUAGE TemplateHaskell #-}
module Tracy.Cameras where

import Control.Applicative
import Control.Lens
import Data.List
import Data.Maybe
import Data.Ord (comparing)
import Data.Colour
import Linear
import Codec.BMP
import qualified Data.ByteString as B

import Tracy.Types
import Tracy.Util

data Camera a =
    Camera { _cameraU :: V3 Double
           , _cameraV :: V3 Double
           , _cameraW :: V3 Double
           , _cameraRenderWorld :: Camera a -> World -> TraceM BMP
           , _cameraData :: a
           , _exposureTime :: Double
           }

data Pinhole =
    Pinhole { _pinholeEyePoint :: V3 Double
            , _pinholeLookatPoint :: V3 Double
            , _pinholeUp :: V3 Double
            , _pinholeVPDistance :: Double
            , _pinholeRayDir :: Camera Pinhole -> V2 Double -> V3 Double
            , _pinholeZoomFactor :: Double
            }

makeLenses ''Camera
makeLenses ''Pinhole

pinholeCamera :: V3 Double -> V3 Double -> V3 Double -> Double -> Double -> Double
              -> Camera Pinhole
pinholeCamera eye look up vpDist exposure z =
    let pinhole = Pinhole eye look up vpDist phRayDir z
        w = signorm $ eye - look
        u = signorm $ up `cross` w
        v = w `cross` u
    in Camera u v w pinholeRender pinhole exposure

phRayDir :: Camera Pinhole -> V2 Double -> V3 Double
phRayDir cam p =
    let ph = cam^.cameraData
    in signorm $ (p^._x *^ cam^.cameraU) +
           (p^._y *^ cam^.cameraV) -
           (ph^.pinholeVPDistance *^ cam^.cameraW)

pinholeRender :: Camera Pinhole -> World -> TraceM BMP
pinholeRender cam w = do
  root <- use (traceConfig.to sampleRoot)
  sampleFunc <- use (traceConfig.to sampler)
  numSets <- use traceNumSampleSets

  sampleSets <- sampleFunc root numSets

  let newPixSize = vp^.pixelSize / cam^.cameraData.pinholeZoomFactor
      vp = w^.viewPlane
      bytes = B.concat $ getColorBytes <$> colors
      colors = concat getRows
      getRows = getRow <$> [0..vp^.vres-1]
      getRow r = getCol r <$> [0..vp^.hres-1]
      getCol row col =
          let sampleSet = sampleSets !! sampleIndex
              sampleIndex = (fromEnum $ row * vp^.hres + col) `mod` numSets
          in (sum (results row col sampleSet) / grey (root * root)) *
             grey (cam^.exposureTime)

      results row col samples = result row col <$> samples
      result row col (sx, sy) =
          let x = newPixSize * (col - (0.5 * vp^.hres) + sx)
              y = newPixSize * (row - (0.5 * vp^.vres) + sy)
              ray = Ray { _origin = cam^.cameraData.pinholeEyePoint
                        , _direction = (cam^.cameraData.pinholeRayDir) cam (V2 x y)
                        }
          in case hitAnObject w ray of
               Nothing -> w^.bgColor
               Just (sh, _t) -> sh^.shadeColor

  logMsg "Tracing using pinhole camera."
  return $ packRGBA32ToBMP (fromEnum $ w^.viewPlane^.hres)
             (fromEnum $ w^.viewPlane^.vres) bytes

hitAnObject :: World -> Ray -> Maybe (Shade, Double)
hitAnObject w r =
    listToMaybe $ sortBy (comparing snd) $ catMaybes results
    where
      results = tests <*> pure r
      tests = w^..objects.folded.hit
