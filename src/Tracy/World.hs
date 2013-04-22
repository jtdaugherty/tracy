{-# LANGUAGE TemplateHaskell #-}
module Tracy.World where

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

logMsg :: String -> TraceM ()
logMsg msg = traceLog %= (msg:)

renderWorld :: World -> TraceM BMP
renderWorld w = do
  root <- use (traceConfig.to sampleRoot)
  sampleFunc <- use (traceConfig.to sampler)
  numSets <- use traceNumSampleSets

  sampleSets <- sampleFunc root numSets

  let vp = w^.viewPlane
      bytes = B.concat $ getColorBytes <$> colors
      colors = concat getRows
      getRows = getRow <$> [0..vp^.vres-1]
      getRow r = getCol r <$> [0..vp^.hres-1]
      getCol row col =
          let sampleSet = sampleSets !! sampleIndex
              sampleIndex = (fromEnum $ row * vp^.hres + col) `mod` numSets
          in sum (results row col sampleSet) / grey (root * root)

      results row col samples = result row col <$> samples
      result row col (sx, sy) =
          let x = vp^.pixelSize * (col - (0.5 * vp^.hres) + sx)
              y = vp^.pixelSize * (row - (0.5 * vp^.vres) + sy)
              rayDir = V3 x y (-1 * w^.viewPlaneDistance)
              ray = Ray { _origin = w^.eyePoint
                        , _direction = signorm rayDir
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
