module Main where

import Control.Concurrent
import Control.Monad
import Control.Lens
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
         | CPUs String
         | Chunks String
         | UseGUI
           deriving (Eq, Show)

data PreConfig =
    PreConfig { argSampleRoot :: Float
              , argAccelScheme :: Maybe AccelScheme
              , argCpuCount :: Int
              , argWorkChunks :: Int
              , argForceShadows :: Maybe Bool
              }

defaultPreConfig :: IO PreConfig
defaultPreConfig = do
    n <- getNumProcessors
    return $ PreConfig { argSampleRoot = 4
                       , argAccelScheme = Nothing
                       , argCpuCount = n
                       , argWorkChunks = 10
                       , argForceShadows = Nothing
                       }

mkOpts :: IO [OptDescr Arg]
mkOpts = do
    maxc <- getNumProcessors
    return [ Option "h" ["help"] (NoArg Help) "This help output"
           , Option "r" ["aa-sample-root"] (ReqArg SampleRoot "ROOT") "AA sample root"
           , Option "n" ["force-no-shadows"] (NoArg NoShadows) "Force shadows off"
           , Option "s" ["force-shadows"] (NoArg Shadows) "Force shadows on"
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
updateConfig c NoShadows = return $ c { argForceShadows = Just True }
updateConfig c Shadows = return $ c { argForceShadows = Just False }
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
  preCfg <- foldM updateConfig defPreCfg os

  when (Help `elem` os) usage
  when (length rest /= 1) usage

  let [toRender] = rest

  setNumCapabilities $ argCpuCount preCfg

  case lookup toRender scenes of
    Nothing -> putStrLn $ "No such scene: " ++ toRender
    Just sceneDesc -> do
        let renderCfg = defaultRenderConfig & sampleRoot .~ (argSampleRoot preCfg)
                                            & forceShadows .~ (argForceShadows preCfg)

            filename = toRender ++ ".bmp"

        putStrLn $ "Rendering " ++ filename ++ " ..."

        iChan <- newChan
        dChan <- newChan

        _ <- forkIO $ consoleHandler iChan
        -- TODO: use (networkRenderThread nodes) here when appropriate
        --_ <- forkIO $ render toRender (argWorkChunks preCfg) renderCfg sceneDesc localRenderThread iChan dChan
        _ <- forkIO $ render toRender (argWorkChunks preCfg) renderCfg sceneDesc (networkRenderThread ["tcp://localhost:12345", "tcp://localhost:12346"] iChan) iChan dChan

        case UseGUI `elem` os of
            False -> fileHandler filename dChan
            True -> do
                dChan2 <- dupChan dChan
                _ <- forkIO $ fileHandler filename dChan2
                guiHandler dChan
