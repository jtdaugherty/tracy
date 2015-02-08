{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tracy.ChunkRender
  ( renderChunk
  )
  where

import Control.Lens
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
            -> (Row, Row)
            -> IO (SV.Vector Color)
renderChunk cfg s tracer sampleData sampleSetIndices sampleRange (Row startRow, Row stopRow) = do
  let cam = s^.sceneCamera
      w = s^.sceneWorld
      renderer = cam^.cameraRenderWorld
      chunkRows = [startRow..stopRow]
      worker p = renderer cam cfg w tracer sampleData p sampleRange

  let r = parMap (rpar `dot` rdeepseq) worker (zip chunkRows sampleSetIndices)
  r `deepseq` return ()

  return $ SV.concat r
