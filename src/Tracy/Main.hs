{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tracy.Main where

import Control.Parallel.Strategies
import Control.Lens
import Control.DeepSeq
import Control.Monad
import Control.Concurrent.Chan
import Data.Time.Clock
import Data.Colour
import Data.Array.IO
import qualified Data.Vector as V
import GHC.Conc
import System.Random

import Tracy.Types
import Tracy.Samplers
import Tracy.Cameras
import Tracy.AccelSchemes

defaultConfig :: IO Config
defaultConfig = do
    n <- getNumProcessors
    return $ Config { _vpSampler = regular
                    , _sampleRoot = 4
                    , _accelScheme = noScheme
                    , _cpuCount = n
                    , _workChunks = 10
                    }

instance NFData Colour where
    rnf (Colour r g b) = r `seq` g `seq` b `seq` ()

shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs

render :: Config -> Camera ThinLens -> World -> Chan InfoEvent -> Chan DataEvent -> IO ()
render cfg cam w iChan dChan = do
  let numSets = fromEnum (w^.viewPlane.hres * 2.3)
      squareSampler = cfg^.vpSampler
      diskSampler = cam^.cameraData.lensSampler
      renderer = cam^.cameraRenderWorld
      numChunks = cfg^.workChunks
      rowsPerChunk = w^.viewPlane.vres / (toEnum numChunks)
      chunk f xs = result : chunk f rest where (result, rest) = f xs
      chunks = filter (not . null) $ take (numChunks + 1) $ chunk (splitAt (fromEnum rowsPerChunk)) rows
      rows = [0..(fromEnum $ w^.viewPlane.vres-1)]

  -- Generate sample data for square and disk samplers
  squareSamples <- V.replicateM numSets $ squareSampler (cfg^.sampleRoot)
  diskSamples <- V.replicateM numSets $ diskSampler (cfg^.sampleRoot)

  let worker = renderer cam numSets cfg w squareSamples diskSamples

  writeChan iChan $ ISampleRoot $ cfg^.sampleRoot
  writeChan iChan $ IAccelSchemeName (cfg^.accelScheme.schemeName)
  writeChan iChan $ INumObjects $ w^.objects.to length
  writeChan iChan $ IShadows $ w^.worldShadows
  writeChan iChan $ INumSquareSampleSets $ V.length squareSamples
  writeChan iChan $ INumDiskSampleSets $ V.length diskSamples
  writeChan iChan $ INumCPUs $ cfg^.cpuCount
  writeChan iChan $ INumRowsPerChunk $ fromEnum rowsPerChunk
  writeChan iChan $ INumChunks $ length chunks
  writeChan iChan $ IImageSize (fromEnum $ w^.viewPlane.hres)
                               (fromEnum $ w^.viewPlane.vres)

  writeChan dChan $ DNumChunks $ length chunks
  writeChan dChan $ DImageSize (fromEnum $ w^.viewPlane.hres)
                               (fromEnum $ w^.viewPlane.vres)

  t1 <- getCurrentTime
  writeChan iChan $ IStartTime t1

  writeChan dChan DStarted
  writeChan iChan IStarted

  forM_ (zip ([1..]::[Int]) chunks) $
    \(chunkId, chunkRows) -> do
        -- Zip up chunkRows values with sets of randomly-generated sample set indices
        indices <- forM chunkRows $ \_ -> shuffle [0..numSets-1]

        let r = parMap (rpar `dot` rdeepseq) worker (zip chunkRows indices)
        r `deepseq` return ()
        t <- getCurrentTime
        let remaining = toEnum $ ((fromEnum $ diffUTCTime t t1) `div` chunkId) * (length chunks - chunkId)
            estimate = if chunkId == 1
                       then Nothing
                       else Just remaining
        writeChan iChan $ IChunkFinished chunkId (length chunks) estimate
        writeChan dChan $ DChunkFinished chunkId r

  t2 <- getCurrentTime

  writeChan dChan DFinished

  writeChan iChan IFinished
  writeChan iChan $ IFinishTime t2
  writeChan iChan $ ITotalTime (diffUTCTime t2 t1)

  writeChan dChan DShutdown
  writeChan iChan IShutdown
