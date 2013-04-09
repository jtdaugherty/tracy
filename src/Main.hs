module Main where

import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.Time.Clock
import Codec.BMP
import System.Console.GetOpt
import System.Environment
import System.Exit

import Tracy.World
import Tracy.Scenes

data Arg = ShowLog
         | BeSilent
         | Help
           deriving (Eq, Show)

data Config =
    Config { showLog :: Bool
           , silent :: Bool
           }
    deriving (Eq, Show)

opts :: [OptDescr Arg]
opts = [ Option "h" ["help"] (NoArg Help) "This help output"
       , Option "l" ["log"] (NoArg ShowLog) "Show log messages"
       , Option "q" ["quiet"] (NoArg BeSilent) "Operate silently (no output)"
       ]

updateConfig :: Config -> Arg -> Config
updateConfig c Help = c
updateConfig c ShowLog = c { showLog = True }
updateConfig c BeSilent = c { showLog = False, silent = True }

defaultConfig :: Config
defaultConfig =
    Config { showLog = False
           , silent = False
           }

usage :: IO ()
usage = do
  pn <- getProgName
  let header = "Usage: " ++ pn ++ " [options]"
  putStrLn $ usageInfo header opts
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  let (os, _, _) = getOpt Permute opts args
      cfg = foldl updateConfig defaultConfig os

  when (Help `elem` os) usage

  let putMsg = when (not $ silent cfg) . putStrLn
      putLog = when (showLog cfg) . putStrLn

  forM_ scenes $ \(filename, w) ->
      do
        putMsg $ "Rendering " ++ filename ++ " ..."
        putMsg $ "  Objects: " ++ (w^.objects^.to length^.to show)

        t1 <- getCurrentTime
        let (img, st) = runState (renderScene w) (TraceState [])
        writeBMP filename img

        when (length (st^.traceLog) > 0) $
             do
               putLog "  Log:"
               forM_ (st^.traceLog) $ \m ->
                   putLog $ "    " ++ m

        t2 <- getCurrentTime

        putMsg $ "done. Total time: " ++ (show $ diffUTCTime t2 t1)
