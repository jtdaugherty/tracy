{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tracy.ChunkRender
  ( renderChunk
  )
  where

import Control.Lens
import Numeric.Lens
import Control.Monad
import Control.Parallel.Strategies
import Control.DeepSeq
import Data.Colour
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import System.Random.MWC

import Tracy.Types

instance NFData Colour where
    rnf (Colour r g b) = r `seq` g `seq` b `seq` ()

renderChunk :: RenderConfig
            -> GenIO
            -> Scene ThinLens
            -> Tracer
            -> SampleData
            -> IO (SV.Vector Color)
renderChunk cfg rng s tracer sampleData = do
  let cam = s^.sceneCamera
      w = s^.sceneWorld
      renderer = cam^.cameraRenderWorld
      chunkRows = [start..stop]
      start = 0
      stop = s^.sceneWorld.viewPlane.vres.subtracting 1.from enum
      worker = renderer cam cfg w tracer sampleData

  -- Zip up chunkRows values with sets of randomly-generated sample set indices
  sampleIndices <- replicateM (stop - start + 1) $
                     replicateM (sampleData^.numSets) $
                       uniformR (0, sampleData^.numSets - 1) rng

  let r = parMap (rpar `dot` rdeepseq) worker (zip chunkRows sampleIndices)
  r `deepseq` return ()

  return $ SV.concat r
