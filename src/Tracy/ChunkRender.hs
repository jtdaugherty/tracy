{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tracy.ChunkRender
  ( renderChunk
  )
  where

import Control.Lens
import Numeric.Lens
import Control.Parallel.Strategies
import Control.DeepSeq
import Data.Colour
import qualified Data.Vector.Storable as SV
import qualified Data.Vector as V

import Tracy.Types

instance NFData Colour where
    rnf (Colour r g b) = r `seq` g `seq` b `seq` ()

renderChunk :: RenderConfig
            -> Scene ThinLens
            -> Tracer
            -> SampleData
            -> [V.Vector Int]
            -> (Int, Int)
            -> IO (SV.Vector Color)
renderChunk cfg s tracer sampleData sampleSetIndices sampleRange = do
  let cam = s^.sceneCamera
      w = s^.sceneWorld
      renderer = cam^.cameraRenderWorld
      chunkRows = [start..stop]
      start = 0
      stop = s^.sceneWorld.viewPlane.vres.subtracting 1.from enum
      worker p = renderer cam cfg w tracer sampleData p sampleRange

  let r = parMap (rpar `dot` rdeepseq) worker (zip chunkRows sampleSetIndices)
  r `deepseq` return ()

  return $ SV.concat r
