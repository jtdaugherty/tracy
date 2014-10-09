{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tracy.ChunkRender
  ( renderChunk
  )
  where

import Control.Lens
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
            -> (Int, Int)
            -> Tracer
            -> V.Vector [(Float, Float)]
            -> V.Vector [(Float, Float)]
            -> V.Vector [(Float, Float)]
            -> IO (SV.Vector Color)
renderChunk cfg rng s (start, stop) tracer sSamples dSamples oSamples = do
  let cam = s^.sceneCamera
      w = s^.sceneWorld
      numSets = V.length sSamples
      renderer = cam^.cameraRenderWorld
      chunkRows = [start..stop]
      worker = renderer cam numSets cfg w tracer sSamples dSamples oSamples

  -- Zip up chunkRows values with sets of randomly-generated sample set indices
  sampleIndices <- replicateM (stop - start + 1) $
                     replicateM numSets $
                       uniformR (0, numSets - 1) rng

  let r = parMap (rpar `dot` rdeepseq) worker (zip chunkRows sampleIndices)
  r `deepseq` return ()

  return $ SV.concat r
