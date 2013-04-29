{-# LANGUAGE TemplateHaskell #-}
module Tracy.Cameras where

import Control.Applicative
import Control.Lens
import Data.List
import Data.Maybe
import Data.Ord (comparing)
import Data.Colour
import qualified Data.Vector as V
import Linear
import GHC.Float

import Tracy.Types

type CameraRenderer a = Camera a
                      -> V.Vector [(Float, Float)]
                      -> V.Vector [(Float, Float)]
                      -> Int
                      -> Config
                      -> Int
                      -> World
                      -> [Color]

data Camera a =
    Camera { _cameraU :: V3 Float
           , _cameraV :: V3 Float
           , _cameraW :: V3 Float
           , _cameraRenderWorld :: CameraRenderer a
           , _cameraData :: a
           , _exposureTime :: Float
           , _cameraZoomFactor :: Float
           , _cameraEyePoint :: V3 Float
           }

data ThinLens =
    ThinLens { _lensRadius :: Float
             , _lensVPDistance :: Float
             , _lensFocalPlaneDistance :: Float
             , _lensRayDir :: Camera ThinLens -> V2 Float -> V2 Float -> V3 Float
             , _lensSampler :: Sampler (Float, Float)
             }

makeLenses ''Camera
makeLenses ''ThinLens

camera :: V3 Float -> V3 Float -> V3 Float -> Float -> Float
       -> a
       -> (CameraRenderer a)
       -> Camera a
camera eye look up exposure z dat render =
    let w = signorm $ eye - look
        u = signorm $ up `cross` w
        v = w `cross` u
    in Camera u v w render dat exposure z eye

thinLensCamera :: V3 Float -> V3 Float -> V3 Float -> Float
               -> Float -> Float -> Float -> Float
               -> Sampler (Float, Float)
               -> Camera ThinLens
thinLensCamera eye look up exposure z vpDist fpDist rad s =
    let thinLens = ThinLens rad vpDist fpDist thinLensRayDir s
    in camera eye look up exposure z thinLens thinLensRender

thinLensRayDir :: Camera ThinLens -> V2 Float -> V2 Float -> V3 Float
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

maxToOne :: Color -> Color
maxToOne (Colour r g b) = Colour r' g' b'
    where
      m = maximum [r, g, b]
      (r', g', b') = if m > 1
                     then (r/m, g/m, b/m)
                     else (r, g, b)

thinLensRender :: CameraRenderer ThinLens
thinLensRender cam squareSampleSets diskSampleSets numSets config theRow w =
  let root  = config^.to sampleRoot
      newPixSize = vp^.pixelSize / cam^.cameraZoomFactor
      vp = w^.viewPlane
      colors = getCol (toEnum theRow) <$> [0..vp^.hres-1]
      getCol row col =
          let squareSampleSet = squareSampleSets V.! sampleIndex
              diskSampleSet = diskSampleSets V.! sampleIndex
              sampleIndex = (fromEnum $ row * vp^.hres + col) `mod` numSets

          in maxToOne ((sum (results row col squareSampleSet diskSampleSet) / grey (float2Double $ root * root)) *
              grey (float2Double $ cam^.exposureTime))

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
               Just (sh, _t) -> (sh^.material.doShading) w (sh & shadeRay .~ ray)

  in colors

hitAnObject :: World -> Ray -> Maybe (Shade, Float)
hitAnObject w r =
    listToMaybe $ sortBy (comparing snd) $ catMaybes results
    where
      results = tests <*> pure r
      tests = w^..objects.folded.hit
