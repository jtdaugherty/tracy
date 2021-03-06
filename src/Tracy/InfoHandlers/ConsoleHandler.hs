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
            ILoadedMeshes (Count n) -> outputS "Loaded meshes" n
            ILoadedTextures (Count n) -> outputS "Loaded textures" n
            ILoadingMeshes -> output_ "Loading meshes..."
            ILoadingTextures -> output_ "Loading textures..."
            INumObjects (Count n) -> outputS "Objects" n
            ITraceMaxDepth (Depth v) -> outputS "Maximum trace depth" v
            IConnected s -> output "Connected to" s
            IConnecting s -> output "Connecting to" s
            INodeReady s -> output "Node ready" s
            IShadows val -> output "Shadows" (if val then "yes" else "no")
            IImageSize (Width w) (Height h) -> output "Image size" (show w ++ "px (W) x " ++ show h ++ "px (H)")
            INumCPUs n -> outputS "Using CPUs" n
            IStartTime t -> outputS "Start time" t
            IFinishTime t -> putStrLn "" >> outputS "Finish time" t
            ITotalTime t -> outputS "Total time" t
            ISettingScene -> output_ "Setting up scene on nodes..."
            IStarted -> output_ "Starting."
            IFrameRange (Frame fa, Frame fb) ->
                case fa == fb of
                    False -> output "Frame range" (show fa ++ "-" ++ show fb)
                    True -> output "Frame" (show fa)
            IChunkFinished (Frame fn) (Count finished) (Count total) t ->
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
                        putStr $ "  Rendering...                   Frame " ++ show fn ++ ": " ++
                          (concat [ show perc, "% (", totalStr , " remaining)     \r" ])
                        hFlush stdout

            IFinished _ -> return ()
            IShutdown -> output_ "Done."
        if ev == IShutdown then
           return False else
           return True
