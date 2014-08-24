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
    leftWidth = 30
    output a b = putStrLn $ "  " ++ a ++ ":" ++ (replicate (leftWidth - length a) ' ') ++ b
    outputS a b = putStrLn $ "  " ++ a ++ ":" ++ (replicate (leftWidth - length a) ' ') ++ show b
    handleEvent = do
        ev <- readChan chan
        case ev of
            ISampleRoot root -> output "Sampler root" ((show root) ++ " (" ++ (show $ root ** 2) ++ " samples per pixel)")
            IAccelSchemeName name -> output "Acceleration method" name
            INumObjects n -> outputS "Objects" n
            IShadows val -> output "Shadows" (if val then "yes" else "no")
            IImageSize w h -> output "Image size" (show w ++ "px (W) x " ++ show h ++ "px (H)")
            INumSquareSampleSets n -> outputS "Square sample sets" n
            INumDiskSampleSets n -> outputS "Disk sample sets" n
            INumCPUs n -> outputS "Using CPUs" n
            INumRowsPerChunk n -> outputS "Pixel rows per chunk" n
            INumChunks n -> outputS "Chunks" n
            IStartTime t -> outputS "Start time" t
            IStarted -> (putStr $ "\r  Rendering:") >> hFlush stdout
            IChunkFinished cId total -> (putStr $ "\r  Rendering: " ++ show cId ++ "/" ++ show total) >> hFlush stdout
            IFinishTime t -> outputS "Finish time" t
            ITotalTime t -> outputS "Total time" t
            IFinished -> putStrLn ""
            IShutdown -> putStrLn "  Done."
        if ev == IShutdown then
           return False else
           return True

