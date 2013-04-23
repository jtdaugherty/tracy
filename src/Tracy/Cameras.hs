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
           , _cameraZoomFactor :: Double
           , _cameraEyePoint :: V3 Double
           }

data Pinhole =
    Pinhole { _pinholeLookatPoint :: V3 Double
            , _pinholeUp :: V3 Double
            , _pinholeVPDistance :: Double
            , _pinholeRayDir :: Camera Pinhole -> V2 Double -> V3 Double
            }

data ThinLens =
    ThinLens { _lensRadius :: Double
             , _lensVPDistance :: Double
             , _lensFocalPlaneDistance :: Double
             , _lensRayDir :: Camera ThinLens -> V2 Double -> V2 Double -> V3 Double
             , _lensSampler :: Sampler (Double, Double)
             }

makeLenses ''Camera
makeLenses ''Pinhole
makeLenses ''ThinLens

camera :: V3 Double -> V3 Double -> V3 Double -> Double -> Double
       -> a
       -> (Camera a -> World -> TraceM BMP)
       -> Camera a
camera eye look up exposure z dat render =
    let w = signorm $ eye - look
        u = signorm $ up `cross` w
        v = w `cross` u
    in Camera u v w render dat exposure z eye

pinholeCamera :: V3 Double -> V3 Double -> V3 Double -> Double -> Double -> Double
              -> Camera Pinhole
pinholeCamera eye look up exposure z vpDist =
    let pinhole = Pinhole look up vpDist phRayDir
    in camera eye look up exposure z pinhole pinholeRender

thinLensCamera :: V3 Double -> V3 Double -> V3 Double -> Double
               -> Double -> Double -> Double -> Double
               -> Sampler (Double, Double)
               -> Camera ThinLens
thinLensCamera eye look up exposure z vpDist fpDist rad s =
    let thinLens = ThinLens rad vpDist fpDist thinLensRayDir s
    in camera eye look up exposure z thinLens thinLensRender

phRayDir :: Camera Pinhole -> V2 Double -> V3 Double
phRayDir cam p =
    let ph = cam^.cameraData
    in signorm $ (p^._x *^ cam^.cameraU) +
           (p^._y *^ cam^.cameraV) -
           (ph^.pinholeVPDistance *^ cam^.cameraW)

thinLensRayDir :: Camera ThinLens -> V2 Double -> V2 Double -> V3 Double
thinLensRayDir cam pixelPoint lensPoint =
    let f = cam^.cameraData^.lensFocalPlaneDistance
        d = cam^.cameraData^.lensVPDistance
        r = f / d
        px = pixelPoint^._x * r
        py = pixelPoint^._y * r
        dir = ((px - lensPoint^._x) *^ cam^.cameraU) +
              ((py - lensPoint^._y) *^ cam^.cameraV) -
              (f *^ cam^.cameraW)

    in signorm dir

thinLensRender :: Camera ThinLens -> World -> TraceM BMP
thinLensRender cam w = do
  root <- use (traceConfig.to sampleRoot)
  vpSampleFunc <- use (traceConfig.to vpSampler)
  numSets <- use traceNumSampleSets

  squareSampleSets <- vpSampleFunc root numSets
  let diskSamplerFunc = cam^.cameraData^.lensSampler
  diskSampleSets <- diskSamplerFunc root numSets

  let newPixSize = vp^.pixelSize / cam^.cameraZoomFactor
      vp = w^.viewPlane
      bytes = B.concat $ getColorBytes <$> colors
      colors = concat getRows
      getRows = getRow <$> [0..vp^.vres-1]
      getRow r = getCol r <$> [0..vp^.hres-1]
      getCol row col =
          let squareSampleSet = squareSampleSets !! sampleIndex
              diskSampleSet = diskSampleSets !! sampleIndex
              sampleIndex = (fromEnum $ row * vp^.hres + col) `mod` numSets

          in (sum (results row col squareSampleSet diskSampleSet) / grey (root * root)) *
             grey (cam^.exposureTime)

      results row col pixelSamples diskSamples = result row col <$> (zip pixelSamples diskSamples)
      result row col ((sx, sy), (dx, dy)) =
          let x = newPixSize * (col - (0.5 * vp^.hres) + sx)
              y = newPixSize * (row - (0.5 * vp^.vres) + sy)

              lx = dx * cam^.cameraData^.lensRadius
              ly = dy * cam^.cameraData^.lensRadius

              o = cam^.cameraEyePoint +
                  (lx *^ cam^.cameraU) +
                  (ly *^ cam^.cameraV)

              d = (cam^.cameraData^.lensRayDir) cam (V2 x y) (V2 lx ly)

              ray = Ray { _origin = o
                        , _direction = d
                        }
          in case hitAnObject w ray of
               Nothing -> w^.bgColor
               Just (sh, _t) -> sh^.shadeColor

  logMsg "Tracing using thin lens camera."
  return $ packRGBA32ToBMP (fromEnum $ w^.viewPlane^.hres)
             (fromEnum $ w^.viewPlane^.vres) bytes

pinholeRender :: Camera Pinhole -> World -> TraceM BMP
pinholeRender cam w = do
  root <- use (traceConfig.to sampleRoot)
  vpSampleFunc <- use (traceConfig.to vpSampler)
  numSets <- use traceNumSampleSets

  sampleSets <- vpSampleFunc root numSets

  let newPixSize = vp^.pixelSize / cam^.cameraZoomFactor
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
              ray = Ray { _origin = cam^.cameraEyePoint
                        , _direction = (cam^.cameraData^.pinholeRayDir) cam (V2 x y)
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
