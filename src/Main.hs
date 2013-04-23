module Main where

import Control.Applicative
import Control.Monad
import System.Console.GetOpt
import System.Environment
import System.Exit

import Tracy.Main
import Tracy.Scenes
import Tracy.Types

data Arg = ShowLog
         | BeSilent
         | Help
         | SampleRoot String
           deriving (Eq, Show)

opts :: [OptDescr Arg]
opts = [ Option "h" ["help"] (NoArg Help) "This help output"
       , Option "l" ["log"] (NoArg ShowLog) "Show log messages"
       , Option "q" ["quiet"] (NoArg BeSilent) "Operate silently (no output)"
       , Option "a" ["aa-sample-root"] (ReqArg SampleRoot "ROOT") "AA sample root"
       ]

updateConfig :: Config -> Arg -> Config
updateConfig c Help = c
updateConfig c ShowLog = c { showLog = True }
updateConfig c BeSilent = c { showLog = False, silent = True }
updateConfig c (SampleRoot s) = c { sampleRoot = read s }

usage :: IO ()
usage = do
  pn <- getProgName
  let header = "Usage: " ++ pn ++ " [options] [scene name]"
  putStrLn $ usageInfo header opts
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  let (os, rest, _) = getOpt Permute opts args
      cfg = foldl updateConfig defaultConfig os

  when (Help `elem` os) usage

  let toRender = if null rest
                 then fst <$> scenes
                 else rest

  forM_ toRender $ \n -> do
         case lookup n scenes of
           Nothing -> putStrLn $ "No such scene: " ++ n
           Just (w, act) -> render cfg w act $ n ++ ".bmp"
