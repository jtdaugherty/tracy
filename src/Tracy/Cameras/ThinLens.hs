{-# LANGUAGE TemplateHaskell, BangPatterns #-}
module Tracy.Cameras.ThinLens
  ( thinLensCamera
  )
  where

import Control.Lens
import Control.Monad.Reader
import Data.Colour
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import Linear

import Tracy.Types
import Tracy.Samplers (toUnitHemi)

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
    let !f = cam^.cameraData^.lensFocalPlaneDistance
        !d = cam^.cameraData^.lensVPDistance
        !r = f / d
        !px = pixelPoint^._x * r
        !py = pixelPoint^._y * r
        !raydir = ((px - lensPoint^._x) *^ cam^.cameraU) +
                  ((py - lensPoint^._y) *^ cam^.cameraV) -
                  (f *^ cam^.cameraW)

    in signorm raydir

thinLensRender :: CameraRenderer ThinLens
thinLensRender cam _ w tracer sampleData (Row theRow, sampleSetIndices) sampleRange =
  let !newPixSize = vp^.pixelSize / cam^.cameraZoomFactor
      !sampleIndices = V.fromList [fst sampleRange .. snd sampleRange]
      !exposure = grey (cam^.exposureTime)
      !vp = w^.viewPlane
      !row = toEnum theRow
      !colors = SV.generate (fromEnum $ vp^.hres) (getCol . toEnum)
      !hitFuncs = w^..objects.folded.hit
      !shadowHitFuncs = w^..objects.folded.shadow_hit
      getCol col =
          let !squareSampleSet = (sampleData^.squareSampleSets) V.! sampleSetIndex
              !diskSampleSet = (sampleData^.diskSampleSets) V.! sampleSetIndex
              !objectSampleSet = (sampleData^.objectSampleSets) V.! sampleSetIndex
              !pixelSampleSet = (sampleData^.pixelSampleSets) V.! sampleSetIndex
              !sampleSetIndex = sampleSetIndices V.! ((fromEnum col) `mod` sampleData^.numSets)

          in (V.sum (results col pixelSampleSet squareSampleSet diskSampleSet objectSampleSet) * exposure)

      results :: Double -> V.Vector (Double, Double) -> V.Vector (Double, Double)
              -> V.Vector (Double, Double) -> V.Vector (Double, Double) -> V.Vector Color
      results col pixelSamples squareSamples diskSamples objectSamples =
          V.map (\idx -> result col (pixelSamples V.! idx)
                                    (squareSamples V.! idx)
                                    (diskSamples V.! idx)
                                    (objectSamples V.! idx)
                ) sampleIndices

      result col (px, py) (sx, sy) (dx, dy) (ox, oy) =
          let !x = newPixSize * (col - (0.5 * vp^.hres) + px)
              !y = newPixSize * (row - (0.5 * vp^.vres) + py)

              !lx = dx * cam^.cameraData.lensRadius
              !ly = dy * cam^.cameraData.lensRadius

              !o = cam^.cameraEyePoint +
                  (lx *^ cam^.cameraU) +
                  (ly *^ cam^.cameraV)

              !d_1 = V2 x y
              !d_2 = V2 lx ly
              !d = (cam^.cameraData.lensRayDir) cam d_1 d_2

              !ray = Ray { _origin = o
                        , _direction = d
                        }
              st = TD { _tdHemiSample = toUnitHemi 1 (sx, sy)
                      , _tdHemiSampleExp = flip toUnitHemi (sx, sy)
                      , _tdDiskSample = V2 dx dy
                      , _tdSquareSample = V2 sx sy
                      , _tdObjectSurfaceSample = V2 ox oy
                      , _tdWorld = w
                      , _tdWorldHitFuncs = hitFuncs
                      , _tdWorldShadowHitFuncs = shadowHitFuncs
                      }
          in runReader ((tracer^.doTrace) ray 0) st
  in colors
