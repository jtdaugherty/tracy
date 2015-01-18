{-# LANGUAGE TemplateHaskell #-}
module Tracy.Cameras
  ( thinLensCamera
  )
  where

import Control.Applicative
import Control.Lens
import Control.Monad.Reader
import Data.Colour
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import Linear
import GHC.Float

import Tracy.Types
import Tracy.Util (max3)
import Tracy.Samplers (toHemi)

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
      m = max3 r g b
      (r', g', b') = if m > 1
                     then (r/m, g/m, b/m)
                     else (r, g, b)

thinLensRender :: CameraRenderer ThinLens
thinLensRender cam config w tracer sampleData (theRow, sampleIndices) =
  let root = config^.sampleRoot
      newPixSize = vp^.pixelSize / cam^.cameraZoomFactor
      maxToOneDenom = grey (float2Double $ root * root)
      maxToOneExposure = grey (float2Double $ cam^.exposureTime)
      vp = w^.viewPlane
      row = toEnum theRow
      colors = SV.generate (fromEnum $ vp^.hres) (getCol . toEnum)
      hitFuncs = w^..objects.folded.hit
      shadowHitFuncs = w^..objects.folded.shadow_hit
      getCol col =
          let squareSampleSet = (sampleData^.squareSampleSets) V.! sampleIndex
              diskSampleSet = (sampleData^.diskSampleSets) V.! sampleIndex
              objectSampleSet = (sampleData^.objectSampleSets) V.! sampleIndex
              sampleIndex = sampleIndices !! ((fromEnum col) `mod` sampleData^.numSets)

          in maxToOne ((V.sum (results col squareSampleSet diskSampleSet objectSampleSet) / maxToOneDenom) *
              maxToOneExposure)

      results :: Float -> V.Vector (Float, Float) -> V.Vector (Float, Float) -> V.Vector (Float, Float) -> V.Vector Color
      results col pixelSamples diskSamples objectSamples =
          V.map (result col) (V.zip3 pixelSamples diskSamples objectSamples)

      result col ((sx, sy), (dx, dy), (ox, oy)) =
          let x = newPixSize * (col - (0.5 * vp^.hres) + sx)
              y = newPixSize * (row - (0.5 * vp^.vres) + sy)

              lx = dx * cam^.cameraData.lensRadius
              ly = dy * cam^.cameraData.lensRadius

              o = cam^.cameraEyePoint +
                  (lx *^ cam^.cameraU) +
                  (ly *^ cam^.cameraV)

              d_1 = V2 x y
              d_2 = V2 lx ly
              d = (cam^.cameraData.lensRayDir) cam d_1 d_2

              ray = Ray { _origin = o
                        , _direction = d
                        }
              st = TD { _tdHemiSample = toHemi 1 (sx, sy)
                      , _tdHemiSampleExp = flip toHemi (sx, sy)
                      , _tdDiskSample = V2 dx dy
                      , _tdSquareSample = V2 sx sy
                      , _tdObjectSurfaceSample = V2 ox oy
                      , _tdWorld = w
                      , _tdWorldHitFuncs = hitFuncs
                      , _tdWorldShadowHitFuncs = shadowHitFuncs
                      }
          in runReader ((tracer^.doTrace) ray 0) st
  in colors
