{-# LANGUAGE TemplateHaskell #-}
module Tracy.Cameras where

import Control.Applicative
import Control.Lens
import Data.List
import Data.Maybe
import Data.Ord (comparing)
import Data.Colour
import Linear

import Tracy.Types

type CameraRenderer a = Camera a
                      -> [[(Double, Double)]]
                      -> [[(Double, Double)]]
                      -> Int
                      -> Config
                      -> Int
                      -> World
                      -> [Color]

data Camera a =
    Camera { _cameraU :: V3 Double
           , _cameraV :: V3 Double
           , _cameraW :: V3 Double
           , _cameraRenderWorld :: CameraRenderer a
           , _cameraData :: a
           , _exposureTime :: Double
           , _cameraZoomFactor :: Double
           , _cameraEyePoint :: V3 Double
           }

data ThinLens =
    ThinLens { _lensRadius :: Double
             , _lensVPDistance :: Double
             , _lensFocalPlaneDistance :: Double
             , _lensRayDir :: Camera ThinLens -> V2 Double -> V2 Double -> V3 Double
             , _lensSampler :: Sampler (Double, Double)
             }

makeLenses ''Camera
makeLenses ''ThinLens

camera :: V3 Double -> V3 Double -> V3 Double -> Double -> Double
       -> a
       -> (CameraRenderer a)
       -> Camera a
camera eye look up exposure z dat render =
    let w = signorm $ eye - look
        u = signorm $ up `cross` w
        v = w `cross` u
    in Camera u v w render dat exposure z eye

thinLensCamera :: V3 Double -> V3 Double -> V3 Double -> Double
               -> Double -> Double -> Double -> Double
               -> Sampler (Double, Double)
               -> Camera ThinLens
thinLensCamera eye look up exposure z vpDist fpDist rad s =
    let thinLens = ThinLens rad vpDist fpDist thinLensRayDir s
    in camera eye look up exposure z thinLens thinLensRender

thinLensRayDir :: Camera ThinLens -> V2 Double -> V2 Double -> V3 Double
thinLensRayDir cam pixelPoint lensPoint =
    let f = cam^.cameraData^.lensFocalPlaneDistance
        d = cam^.cameraData^.lensVPDistance
        r = f / d
        px = pixelPoint^._x * r
        py = pixelPoint^._y * r
        raydir = ((px - lensPoint^._x) *^ cam^.cameraU) +
                 ((py - lensPoint^._y) *^ cam^.cameraV) -
                 (f *^ cam^.cameraW)

    in signorm raydir

clampColor :: Color -> Color
clampColor (Colour r g b) = Colour r' g' b'
    where
      (r', g', b') = if r > 1 || g > 1 || b > 1
                     then (1, 0, 0)
                     else (r, g, b)

thinLensRender :: CameraRenderer ThinLens
thinLensRender cam squareSampleSets diskSampleSets numSets config theRow w =
  let root  = config^.to sampleRoot
      newPixSize = vp^.pixelSize / cam^.cameraZoomFactor
      vp = w^.viewPlane
      colors = getCol (toEnum theRow) <$> [0..vp^.hres-1]
      getCol row col =
          let squareSampleSet = squareSampleSets !! sampleIndex
              diskSampleSet = diskSampleSets !! sampleIndex
              sampleIndex = (fromEnum $ row * vp^.hres + col) `mod` numSets

          in clampColor ((sum (results row col squareSampleSet diskSampleSet) / grey (root * root)) *
              grey (cam^.exposureTime))

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
               Just (sh, _t) -> (sh^.material.doShading) w sh

  in colors

hitAnObject :: World -> Ray -> Maybe (Shade, Double)
hitAnObject w r =
    listToMaybe $ sortBy (comparing snd) $ catMaybes results
    where
      results = tests <*> pure r
      tests = w^..objects.folded.hit
