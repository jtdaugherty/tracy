module Tracy.ConsoleHandler where

import Control.Concurrent.Chan
import Control.Monad
import System.IO

import Tracy.Types

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

