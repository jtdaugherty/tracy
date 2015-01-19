module Tracy.InfoHandlers.ConsoleHandler
  ( consoleHandler
  )
  where

import Control.Concurrent.Chan
import Control.Monad
-- Import this module to get an orphan instance of Show for UTCTime :(
import Data.Time.LocalTime ()

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
            ISceneName n -> output "Scene name" n
            IFrameNum n -> output "Frame number" (show n)
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
            INumObjects n -> outputS "Objects" n
            IConnected s -> output "Connected to" s
            IConnecting s -> output "Connecting to" s
            IShadows val -> output "Shadows" (if val then "yes" else "no")
            IImageSize w h -> output "Image size" (show w ++ "px (W) x " ++ show h ++ "px (H)")
            INumCPUs n -> outputS "Using CPUs" n
            INumBatches n -> outputS "Batches" n
            IStartTime t -> outputS "Start time" t
            IStarted -> return ()
            IBatchFinished finished total t ->
                    let totalSecs = fromEnum t `div` 1000000000000
                        h = totalSecs `div` 3600
                        m = (totalSecs `mod` 3600) `div` 60
                        s = (totalSecs `mod` 3600) `mod` 60
                        totalStr = concat [ show h
                                          , "h "
                                          , show m
                                          , "m "
                                          , show s
                                          , "s"
                                          ]
                    in output "Finished batch" (concat [ show finished, "/", show total, ", ", totalStr , " remaining" ])
            IFinishTime t -> outputS "Finish time" t
            ITotalTime t -> outputS "Total time" t
            IFinished -> return ()
            IShutdown -> putStrLn "  Done."
        if ev == IShutdown then
           return False else
           return True

