module Tracy.InfoHandlers.ConsoleHandler
  ( consoleHandler
  )
  where

import Control.Concurrent.Chan
import Control.Monad
-- Import this module to get an orphan instance of Show for UTCTime :(
import Data.Time.LocalTime ()
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
    output_ a = putStrLn $ "  " ++ a
    handleEvent = do
        ev <- readChan chan
        case ev of
            ISceneName n -> output "Scene name" n
            ISampleRoot root -> let sStr = if root == 1
                                           then "sample"
                                           else "samples"
                                    msg = concat [ show root
                                                 , " ("
                                                 , show $ root ** 2
                                                 , " "
                                                 , sStr
                                                 , " per pixel)"
                                                 ]
                                in output "Sampler root" msg
            IAccelScheme name -> output "Acceleration method" (show name)
            ILoadedMeshes (Count n) -> outputS "Loaded meshes" n
            ILoadingMeshes -> output_ "Loading meshes..."
            INumObjects (Count n) -> outputS "Objects" n
            IConnected s -> output "Connected to" s
            IConnecting s -> output "Connecting to" s
            INodeReady s -> output "Node ready" s
            IShadows val -> output "Shadows" (if val then "yes" else "no")
            IImageSize (Width w) (Height h) -> output "Image size" (show w ++ "px (W) x " ++ show h ++ "px (H)")
            INumCPUs n -> outputS "Using CPUs" n
            IStartTime t -> outputS "Start time" t
            IFinishTime t -> outputS "Finish time" t
            ITotalTime t -> outputS "Total time" t
            ISettingScene -> output_ "Setting up scene on nodes..."
            IStarted -> output_ "Starting."
            IChunkFinished (Count finished) (Count total) t ->
                    let totalSecs = fromEnum t `div` 1000000000000
                        h = totalSecs `div` 3600
                        m = (totalSecs `mod` 3600) `div` 60
                        s = (totalSecs `mod` 3600) `mod` 60
                        perc = fromEnum $ 100.0 * (toEnum finished/(toEnum total)::Double)
                        totalStr = concat [ show h
                                          , "h "
                                          , show m
                                          , "m "
                                          , show s
                                          , "s"
                                          ]
                    in do
                        putStr $ "  Rendering...                   " ++
                          (concat [ show perc, "% (", totalStr , " remaining)     \r" ])
                        when (finished == total) $ putStrLn ""
                        hFlush stdout

            IFinished (Frame frame) -> output "Finished frame" (show frame)
            IShutdown -> output_ "Done."
        if ev == IShutdown then
           return False else
           return True

