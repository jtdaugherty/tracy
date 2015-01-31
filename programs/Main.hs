module Main where

import Control.Concurrent
import Control.Monad
import Control.Lens
import System.Console.GetOpt
import System.Environment
import System.Exit
import GHC.Conc

import Tracy.Main
import Tracy.Types
import Tracy.SceneLoader

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
         | UseGUI
         | RenderNode String
         | FrameNum String
           deriving (Eq, Show)

data PreConfig =
    PreConfig { argSampleRoot :: Double
              , argAccelScheme :: Maybe AccelScheme
              , argCpuCount :: Int
              , argFrameNum :: Int
              , argForceShadows :: Maybe Bool
              , argRenderNodes :: [String]
              }

defaultPreConfig :: IO PreConfig
defaultPreConfig = do
    n <- getNumProcessors
    return $ PreConfig { argSampleRoot = 1
                       , argAccelScheme = Nothing
                       , argCpuCount = n
                       , argForceShadows = Nothing
                       , argRenderNodes = []
                       , argFrameNum = 1
                       }

mkOpts :: IO [OptDescr Arg]
mkOpts = do
    maxc <- getNumProcessors
    return [ Option "h" ["help"] (NoArg Help) "This help output"
           , Option "r" ["aa-sample-root"] (ReqArg SampleRoot "ROOT") "ROOT^2 samples per pixel"
           , Option "n" ["force-no-shadows"] (NoArg NoShadows) "Force shadows off"
           , Option "s" ["force-shadows"] (NoArg Shadows) "Force shadows on"
           , Option "c" ["cpu-count"] (ReqArg CPUs "COUNT")
             ("Use COUNT CPUs for rendering in parallel (default, max: " ++ show maxc ++ ")")
           , Option "g" ["gui"] (NoArg UseGUI)
             ("Show the rendering in a GUI as it completes")
           , Option "d" ["distribute"] (ReqArg RenderNode "NODE")
             ("Render on NODE (specify once for each node)")
           , Option "f" ["frame"] (ReqArg FrameNum "NUM")
             ("Render animation sequence frame number NUM")
           ]

updateConfig :: PreConfig -> Arg -> IO PreConfig
updateConfig c Help = return c
updateConfig c UseGUI = return c
updateConfig c (SampleRoot s) = return $ c { argSampleRoot = read s }
updateConfig c NoShadows = return $ c { argForceShadows = Just True }
updateConfig c Shadows = return $ c { argForceShadows = Just False }
updateConfig c (RenderNode n) = return $ c { argRenderNodes = n : argRenderNodes c }
updateConfig c (FrameNum s) =
    case reads s of
        [(f, _)] -> return $ c { argFrameNum = f }
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

  result <- loadSceneDesc toRender
  case result of
      Left e -> putStrLn ("Could not load scene: " ++ e) >> exitFailure
      Right sceneDesc -> do
        let renderCfg = defaultRenderConfig & sampleRoot .~ (argSampleRoot preCfg)
                                            & forceShadows .~ (argForceShadows preCfg)

        iChan <- newChan
        dChan <- newChan

        let manager = if null $ argRenderNodes preCfg
                      then localRenderManager
                      else networkRenderManager (argRenderNodes preCfg) iChan

        _ <- forkIO $ consoleHandler iChan
        _ <- forkIO $ render toRender renderCfg sceneDesc (argFrameNum preCfg) manager iChan dChan

        case UseGUI `elem` os of
            False -> do
                let filename = buildFilename toRender (argFrameNum preCfg)
                fileHandler filename dChan
            True -> guiHandler dChan
