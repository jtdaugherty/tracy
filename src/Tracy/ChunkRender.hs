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
import System.Random

import Tracy.Types

instance NFData Colour where
    rnf (Colour r g b) = r `seq` g `seq` b `seq` ()

renderChunk :: RenderConfig -> Scene ThinLens -> (Int, Int) -> [[(Float, Float)]] -> [[(Float, Float)]] -> IO [[Color]]
renderChunk cfg s (start, stop) sSamples dSamples = do
  let cam = s^.sceneCamera
      w = s^.sceneWorld
      numSets = fromEnum $ w^.viewPlane.hres
      renderer = cam^.cameraRenderWorld
      chunkRows = [start..stop]
      squareSamples = V.fromList sSamples
      diskSamples = V.fromList dSamples
      worker = renderer cam numSets cfg w squareSamples diskSamples

  -- Zip up chunkRows values with sets of randomly-generated sample set indices
  sampleIndices <- replicateM (stop - start + 1) $
                     replicateM numSets $
                       randomRIO (0, numSets - 1)

  let r = parMap (rpar `dot` rdeepseq) worker (zip chunkRows sampleIndices)
  r `deepseq` return ()

  return r
