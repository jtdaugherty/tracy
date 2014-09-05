module Tracy.InfoHandlers.ConsoleHandler
  ( consoleHandler
  )
  where

import Control.Concurrent.Chan
import Control.Monad
import Data.Time.Format
import System.Locale

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
            ISampleRoot root -> output "Sampler root" ((show root) ++ " (" ++ (show $ root ** 2) ++ " samples per pixel)")
            IAccelScheme name -> output "Acceleration method" (show name)
            INumObjects n -> outputS "Objects" n
            IConnected s -> output "Connected to" s
            IConnecting s -> output "Connecting to" s
            IShadows val -> output "Shadows" (if val then "yes" else "no")
            IImageSize w h -> output "Image size" (show w ++ "px (W) x " ++ show h ++ "px (H)")
            INumCPUs n -> outputS "Using CPUs" n
            INumRowsPerChunk n -> outputS "Pixel rows per chunk" n
            INumChunks n -> outputS "Chunks" n
            IStartTime t -> outputS "Start time" (formatTime defaultTimeLocale rfc822DateFormat t)
            IStarted -> return ()
            IChunkFinished cId total t ->
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
                    in output "Finished chunk"
                              (concat [ show cId
                                      , "/"
                                      , show total
                                      , " ("
                                      , totalStr
                                      , " remaining)"
                                      ])
            IFinishTime t -> outputS "Finish time" t
            ITotalTime t -> outputS "Total time" t
            IFinished -> return ()
            IShutdown -> putStrLn "  Done."
        if ev == IShutdown then
           return False else
           return True

