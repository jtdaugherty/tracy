module Main where

import Control.Monad
import Control.Lens
import Data.List (intercalate)
import System.Console.GetOpt
import System.Environment
import System.Exit
import GHC.Conc

import Tracy.Main
import Tracy.Scenes
import Tracy.Types
import Tracy.FileHandler
import Tracy.GUIHandler
import Tracy.ConsoleHandler

data Arg = Help
         | SampleRoot String
         | NoShadows
         | Shadows
         | SchemeArg String
         | CPUs String
         | Chunks String
         | UseGUI
           deriving (Eq, Show)

mkOpts :: IO [OptDescr Arg]
mkOpts = do
    maxc <- getNumProcessors
    return [ Option "h" ["help"] (NoArg Help) "This help output"
           , Option "r" ["aa-sample-root"] (ReqArg SampleRoot "ROOT") "AA sample root"
           , Option "n" ["force-no-shadows"] (NoArg NoShadows) "Force shadows off"
           , Option "s" ["force-shadows"] (NoArg Shadows) "Force shadows on"
           , Option "a" ["accel"] (ReqArg SchemeArg "SCHEME")
             ("Acceleration scheme\nValid options:\n " ++ intercalate "\n " (accelSchemes^..folded.schemeName))
           , Option "c" ["cpu-count"] (ReqArg CPUs "COUNT")
             ("Number of CPUs to use (max: " ++ show maxc ++ ")")
           , Option "k" ["chunks"] (ReqArg Chunks "COUNT")
             ("Number of work chunks to use")
           , Option "g" ["gui"] (NoArg UseGUI)
             ("Present a graphical interface during rendering")
           ]

updateConfig :: Config -> Arg -> IO Config
updateConfig c Help = return c
updateConfig c UseGUI = return c
updateConfig c (SampleRoot s) = return $ c & sampleRoot .~ read s
updateConfig c NoShadows = return c
updateConfig c Shadows = return c
updateConfig c (Chunks s) =
    case reads s of
        [(cnt, _)] -> return $ c & workChunks .~ cnt
        _ -> usage >> exitFailure
updateConfig c (CPUs s) = do
    case reads s of
        [(cnt, _)] -> do
            avail <- getNumProcessors
            when (cnt < 1 || cnt > avail) $ do
                putStrLn $ concat [ "Error:\n"
                                  , "Requested CPUs: " ++ show cnt
                                  , "\nAvailable: " ++ show avail
                                  ]
                exitFailure
            return $ c & cpuCount .~ cnt
        _ -> usage >> exitFailure
updateConfig c (SchemeArg s) = do
    case [sch | sch <- accelSchemes, sch^.schemeName == s] of
        [] -> usage
        [v] -> return $ c & accelScheme .~ v
        _ -> error "BUG: too many acceleration schemes matched!"

usage :: IO a
usage = do
  pn <- getProgName
  opts <- mkOpts
  let header = "Usage: " ++ pn ++ " [options] <scene name>"
  putStrLn $ usageInfo header opts
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  opts <- mkOpts
  let (os, rest, _) = getOpt Permute opts args

  defCfg <- defaultConfig
  cfg <- foldM updateConfig defCfg os

  when (Help `elem` os) usage

  when (length rest /= 1) usage

  let forceShadows = if Shadows `elem` os
                     then Just True
                     else if NoShadows `elem` os
                          then Just False
                          else Nothing
      [toRender] = rest

  setNumCapabilities $ cfg^.cpuCount

  case lookup toRender scenes of
    Nothing -> putStrLn $ "No such scene: " ++ toRender
    Just (c, w) -> do
        let w1 = (cfg^.accelScheme.schemeApply) w
            w2 = case forceShadows of
                   Nothing -> w1
                   Just v -> w1 & worldShadows .~ v
            filename = toRender ++ ".bmp"

            dataHandler = if UseGUI `elem` os
                          then guiFileHandler filename
                          else fileHandler filename

        putStrLn $ "Rendering " ++ filename ++ " ..."
        render cfg c w2 consoleHandler dataHandler
