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

import Tracy.DataHandlers.FileHandler
import Tracy.DataHandlers.GUIHandler
import Tracy.InfoHandlers.ConsoleHandler

import Tracy.RenderManagers.Local
import Tracy.RenderManagers.Network

data Arg = Help
         | SampleRoot String
         | NoShadows
         | Shadows
         | CPUs String
         | Frames String
         | UseGUI
         | RenderNode String
           deriving (Eq, Show)

data PreConfig =
    PreConfig { argSampleRoot :: Float
              , argAccelScheme :: Maybe AccelScheme
              , argCpuCount :: Int
              , argWorkFrames :: Int
              , argForceShadows :: Maybe Bool
              , argRenderNodes :: [String]
              }

defaultPreConfig :: IO PreConfig
defaultPreConfig = do
    n <- getNumProcessors
    return $ PreConfig { argSampleRoot = 4
                       , argAccelScheme = Nothing
                       , argCpuCount = n
                       , argWorkFrames = 10
                       , argForceShadows = Nothing
                       , argRenderNodes = []
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
           , Option "f" ["frames"] (ReqArg Frames "COUNT")
             ("Number of frames to render")
           , Option "g" ["gui"] (NoArg UseGUI)
             ("Present a graphical interface during rendering")
           , Option "d" ["distribute"] (ReqArg RenderNode "NODE")
             ("Render the job in parallel on this node (specify once for each node)")
           ]

updateConfig :: PreConfig -> Arg -> IO PreConfig
updateConfig c Help = return c
updateConfig c UseGUI = return c
updateConfig c (SampleRoot s) = return $ c { argSampleRoot = read s }
updateConfig c NoShadows = return $ c { argForceShadows = Just True }
updateConfig c Shadows = return $ c { argForceShadows = Just False }
updateConfig c (RenderNode n) = return $ c { argRenderNodes = n : argRenderNodes c }
updateConfig c (Frames s) =
    case reads s of
        [(cnt, _)] -> return $ c { argWorkFrames = cnt }
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

  case lookup toRender allScenes of
    Nothing -> putStrLn $ "No such scene: " ++ toRender
    Just loadSceneDesc -> do
        sceneDesc <- loadSceneDesc

        let renderCfg = defaultRenderConfig & sampleRoot .~ (argSampleRoot preCfg)
                                            & forceShadows .~ (argForceShadows preCfg)

            filename = toRender ++ ".bmp"

        putStrLn $ "Rendering " ++ filename ++ " ..."

        iChan <- newChan
        dChan <- newChan

        let manager = if null $ argRenderNodes preCfg
                      then localRenderManager
                      else networkRenderManager (argRenderNodes preCfg) iChan

        _ <- forkIO $ consoleHandler iChan
        _ <- forkIO $ render toRender (argWorkFrames preCfg) renderCfg sceneDesc manager iChan dChan

        case UseGUI `elem` os of
            False -> fileHandler filename dChan
            True -> do
                dChan2 <- dupChan dChan
                _ <- forkIO $ fileHandler filename dChan2
                guiHandler dChan
