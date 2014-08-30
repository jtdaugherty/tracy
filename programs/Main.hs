module Main where

import Control.Applicative
import Control.Concurrent
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
import Tracy.AccelSchemes
import Tracy.SceneBuilder

data Arg = Help
         | SampleRoot String
         | NoShadows
         | Shadows
         | SchemeArg String
         | CPUs String
         | Chunks String
         | UseGUI
           deriving (Eq, Show)

data PreConfig =
    PreConfig { argSampleRoot :: Float
              , argAccelScheme :: Maybe AccelScheme
              , argCpuCount :: Int
              , argWorkChunks :: Int
              }

defaultPreConfig :: IO PreConfig
defaultPreConfig = do
    n <- getNumProcessors
    return $ PreConfig { argSampleRoot = 4
                       , argAccelScheme = Nothing
                       , argCpuCount = n
                       , argWorkChunks = 10
                       }

mkOpts :: IO [OptDescr Arg]
mkOpts = do
    maxc <- getNumProcessors
    return [ Option "h" ["help"] (NoArg Help) "This help output"
           , Option "r" ["aa-sample-root"] (ReqArg SampleRoot "ROOT") "AA sample root"
           , Option "n" ["force-no-shadows"] (NoArg NoShadows) "Force shadows off"
           , Option "s" ["force-shadows"] (NoArg Shadows) "Force shadows on"
           , Option "a" ["accel"] (ReqArg SchemeArg "SCHEME")
             ("Override scene-specific acceleration scheme\nValid options:\n " ++ intercalate "\n " (accelSchemes^..folded.schemeName))
           , Option "c" ["cpu-count"] (ReqArg CPUs "COUNT")
             ("Number of CPUs to use (max: " ++ show maxc ++ ")")
           , Option "k" ["chunks"] (ReqArg Chunks "COUNT")
             ("Number of work chunks to use")
           , Option "g" ["gui"] (NoArg UseGUI)
             ("Present a graphical interface during rendering")
           ]

updateConfig :: PreConfig -> Arg -> IO PreConfig
updateConfig c Help = return c
updateConfig c UseGUI = return c
updateConfig c (SampleRoot s) = return $ c { argSampleRoot = read s }
updateConfig c NoShadows = return c
updateConfig c Shadows = return c
updateConfig c (Chunks s) =
    case reads s of
        [(cnt, _)] -> return $ c { argWorkChunks = cnt }
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
            return $ c { argCpuCount = cnt }
        _ -> usage >> exitFailure
updateConfig c (SchemeArg s) = do
    case [sch | sch <- accelSchemes, sch^.schemeName == s] of
        [] -> usage
        [v] -> return $ c { argAccelScheme = Just v }
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

  defPreCfg <- defaultPreConfig
  defCfg <- defaultConfig
  preCfg <- foldM updateConfig defPreCfg os

  when (Help `elem` os) usage

  when (length rest /= 1) usage

  let forceShadows = if Shadows `elem` os
                     then Just True
                     else if NoShadows `elem` os
                          then Just False
                          else Nothing
      [toRender] = rest

  setNumCapabilities $ argCpuCount preCfg

  case lookup toRender scenes of
    Nothing -> putStrLn $ "No such scene: " ++ toRender
    Just sceneDesc -> do
        let Right s = sceneFromDesc sceneDesc
            Just aScheme = (argAccelScheme preCfg) <|> (Just $ s^.sceneAccelScheme)
            cfg = defCfg & sampleRoot .~ (argSampleRoot preCfg)
                         & accelScheme .~ aScheme
                         & cpuCount .~ (argCpuCount preCfg)
                         & workChunks .~ (argWorkChunks preCfg)

            worldAccel = (aScheme^.schemeApply) (s^.sceneWorld)
            worldAccelShadows = case forceShadows of
                                  Nothing -> worldAccel
                                  Just v -> worldAccel & worldShadows .~ v
            filename = toRender ++ ".bmp"

        putStrLn $ "Rendering " ++ filename ++ " ..."

        iChan <- newChan
        dChan <- newChan

        _ <- forkIO $ consoleHandler iChan
        _ <- forkIO $ render cfg (s^.sceneCamera) worldAccelShadows iChan dChan

        case UseGUI `elem` os of
            False -> fileHandler filename dChan
            True -> do
                dChan2 <- dupChan dChan
                _ <- forkIO $ fileHandler filename dChan2
                guiHandler dChan
