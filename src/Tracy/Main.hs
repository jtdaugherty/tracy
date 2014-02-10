{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tracy.Main where

import Control.Applicative
import Control.Parallel.Strategies
import Control.Lens
import Control.DeepSeq
import Control.Monad
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Codec.BMP
import Data.List
import Data.Time.Clock
import Data.Colour
import System.IO
import qualified Data.ByteString as B
import qualified Data.Vector as V
import GHC.Conc

import Tracy.Types
import Tracy.Samplers
import Tracy.Cameras
import Tracy.Util
import Tracy.Grid

defaultConfig :: IO Config
defaultConfig = do
    n <- getNumProcessors
    return $ Config { _vpSampler = regular
                    , _sampleRoot = 4
                    , _accelScheme = gridScheme
                    , _cpuCount = n
                    , _workChunks = 10
                    }

instance NFData Colour where
    rnf (Colour r g b) = r `seq` g `seq` b `seq` ()

noScheme :: AccelScheme
noScheme = AccelScheme "none" id

accelSchemes :: [AccelScheme]
accelSchemes =
    [ noScheme
    , gridScheme
    ]

fileHandler :: FilePath -> Chan DataEvent -> IO ()
fileHandler filename chan = do
  DNumChunks chunks <- readChan chan
  DImageSize cols rows <- readChan chan

  DStarted <- readChan chan

  result <- forM [1..chunks] $ \_ -> do
      DChunkFinished ch rs <- readChan chan
      return (ch, rs)

  DFinished <- readChan chan

  let imgBytes = B.concat $ B.concat <$> (getColorBytes <$>) <$> (concat $ snd <$> sort result)
      img = packRGBA32ToBMP (fromEnum rows) (fromEnum cols) imgBytes

  writeBMP filename img

consoleHandler :: Chan InfoEvent -> IO ()
consoleHandler chan = do
  cont <- handleEvent
  when cont $ consoleHandler chan

  where
    handleEvent = do
        ev <- readChan chan
        case ev of
            ISampleRoot root -> putStrLn $ "  Sampler root: " ++ (show root) ++ " (" ++ (show $ root ** 2) ++ " samples per pixel)"
            IAccelSchemeName name -> putStrLn $ "  Acceleration: " ++ name
            INumObjects n -> putStrLn $ "  Objects: " ++ show n
            IShadows val -> putStrLn $ "  Shadows: " ++ (if val then "yes" else "no")
            IImageSize w h -> putStrLn $ "  Image size: " ++ show w ++ "px (W) x " ++ show h ++ "px (H)"
            INumSquareSampleSets n -> putStrLn $ "  Square sample sets: " ++ show n
            INumDiskSampleSets n -> putStrLn $ "  Disk sample sets: " ++ show n
            INumCPUs n -> putStrLn $ "  Using CPUs: " ++ show n
            INumRowsPerChunk n -> putStrLn $ "  Pixel rows per chunk: " ++ show n
            INumChunks n -> putStrLn $ "  Chunks: " ++ show n
            IStartTime t -> putStrLn $ "  Start time: " ++ show t
            IStarted -> (putStr $ "\r  Rendering:") >> hFlush stdout
            IChunkFinished cId total -> (putStr $ "\r  Rendering: " ++ show cId ++ "/" ++ show total) >> hFlush stdout
            IFinishTime t -> putStrLn $ "  Finish time: " ++ show t
            ITotalTime t -> putStrLn $ "  Total time: " ++ show t
            IFinished -> putStrLn ""
            IShutdown -> putStrLn "  Done."
        if ev == IShutdown then
           return False else
           return True

render :: Config -> Camera ThinLens -> World -> (Chan InfoEvent -> IO ()) -> (Chan DataEvent -> IO ()) -> IO ()
render cfg cam w iHandler dHandler = do
  iChan <- newChan
  dChan <- newChan
  iVar <- newEmptyMVar
  dVar <- newEmptyMVar
  _ <- forkIO $ iHandler iChan >> putMVar iVar ()
  _ <- forkIO $ dHandler dChan >> putMVar dVar ()

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

  let worker r = renderer cam squareSamples diskSamples numSets cfg r w

  writeChan iChan $ ISampleRoot $ cfg^.sampleRoot
  writeChan iChan $ IAccelSchemeName $ cfg^.accelScheme.schemeName
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
        let r = parMap (rpar `dot` rdeepseq) worker chunkRows
        r `deepseq` return ()
        writeChan iChan $ IChunkFinished chunkId (length chunks)
        writeChan dChan $ DChunkFinished chunkId r

  t2 <- getCurrentTime

  writeChan dChan DFinished

  writeChan iChan IFinished
  writeChan iChan $ IFinishTime t2
  writeChan iChan $ ITotalTime (diffUTCTime t2 t1)

  writeChan dChan DShutdown
  writeChan iChan IShutdown

  takeMVar iVar
  takeMVar dVar
