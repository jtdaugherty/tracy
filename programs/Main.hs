{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Applicative ((<$>))
import Control.Concurrent
import Control.Monad
import Control.Lens
import Data.Default
import Data.Monoid
import qualified Data.Map as M
import System.Console.GetOpt
import System.Environment
import System.Exit
import GHC.Conc
import Data.Time.Clock

import Tracy.Main hiding (render)
import qualified Tracy.Main as TM
import Tracy.Types
import Tracy.SceneLoader

import Tracy.DataHandlers.FileHandler
import Tracy.DataHandlers.GLFWHandler

import Tracy.RenderManagers.Local
import Tracy.RenderManagers.Network

import Graphics.Vty
import Brick.Main
import Brick.Util
import Brick.AttrMap
import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.ProgressBar
import Brick.Widgets.Border
import Brick.Widgets.Border.Style

data Arg = Help
         | SampleRoot String
         | NoShadows
         | Shadows
         | TraceMaxDepth String
         | CPUs String
         | SamplesPerChunk String
         | RowsPerChunk String
         | UseGUI
         | RenderNode String
         | FrameStart String
         | FrameStop String
         | ModeDepthFirst
         | ModeBreadthFirst
           deriving (Eq, Show)

data PreConfig =
    PreConfig { argSampleRoot :: Double
              , argCpuCount :: Int
              , argFrameStart :: Int
              , argFrameStop :: Maybe Int
              , argForceShadows :: Maybe Bool
              , argRenderNodes :: [String]
              , argSamplesPerChunk :: Int
              , argRowsPerChunk :: Int
              , argRenderMode :: RenderMode
              , argTraceMaxDepth :: Maybe Depth
              }

defaultPreConfig :: IO PreConfig
defaultPreConfig = do
    n <- getNumProcessors
    return $ PreConfig { argSampleRoot = 1
                       , argCpuCount = n
                       , argForceShadows = Nothing
                       , argRenderNodes = []
                       , argFrameStart = 1
                       , argFrameStop = Nothing
                       , argSamplesPerChunk = 1
                       , argRowsPerChunk = 100
                       , argRenderMode = DepthFirst
                       , argTraceMaxDepth = Nothing
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
           , Option "f" ["start-frame"] (ReqArg FrameStart "NUM")
             ("Render animation frames starting at frame number NUM")
           , Option "t" ["stop-frame"] (ReqArg FrameStop "NUM")
             ("Render animation frames stopping at frame number NUM")
           , Option "R" ["rows-per-chunk"] (ReqArg RowsPerChunk "COUNT")
             "Rows per chunk"
           , Option "S" ["samples-per-chunk"] (ReqArg SamplesPerChunk "COUNT")
             "Samples per chunk"
           , Option "B" ["breadth-first"] (NoArg ModeBreadthFirst)
             "Render chunks breadth-first"
           , Option "D" ["depth-first"] (NoArg ModeDepthFirst)
             "Render chunks depth-first"
           , Option "" ["max-depth"] (ReqArg TraceMaxDepth "DEPTH")
             "Set maximum depth to DEPTH bounces for depth-based tracers"
           ]

updateConfig :: PreConfig -> Arg -> IO PreConfig
updateConfig c Help = return c
updateConfig c UseGUI = return c
updateConfig c (SampleRoot s) = return $ c { argSampleRoot = read s }
updateConfig c NoShadows = return $ c { argForceShadows = Just True }
updateConfig c Shadows = return $ c { argForceShadows = Just False }
updateConfig c ModeDepthFirst = return $ c { argRenderMode = DepthFirst }
updateConfig c ModeBreadthFirst = return $ c { argRenderMode = BreadthFirst }
updateConfig c (RenderNode n) = return $ c { argRenderNodes = n : argRenderNodes c }
updateConfig c (TraceMaxDepth s) =
    case reads s of
        [(f, _)] -> return $ c { argTraceMaxDepth = Just $ Depth f }
        _ -> usage >> exitFailure
updateConfig c (SamplesPerChunk s) =
    case reads s of
        [(f, _)] -> return $ c { argSamplesPerChunk = f }
        _ -> usage >> exitFailure
updateConfig c (RowsPerChunk s) =
    case reads s of
        [(f, _)] -> return $ c { argRowsPerChunk = f }
        _ -> usage >> exitFailure
updateConfig c (FrameStop s) =
    case reads s of
        [(f, _)] -> if f <= 0 then exitFailure else return $ c { argFrameStop = Just f }
        _ -> usage >> exitFailure
updateConfig c (FrameStart s) =
    case reads s of
        [(f, _)] -> if f <= 0 then exitFailure else return $ c { argFrameStart = f }
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
  let header = "Usage: " ++ pn ++ " [options] <scene file>"
  putStrLn $ usageInfo header opts
  exitFailure

data AppEvent = Info InfoEvent
              | VtyEvent Event
              | GUIShutdown

data St =
    St { _renderConfig :: RenderConfig
       , _preConfig :: PreConfig
       , _infoState :: InfoState
       }

data NodeState =
    Connecting
    | Connected
    | Ready
    deriving (Show)

data InfoState =
    InfoState { _sceneName :: Maybe String
              , _frameRange :: Maybe (Frame, Frame)
              , _numObjects :: Maybe Count
              , _shadows :: Maybe Bool
              , _numCPUs :: Maybe Count
              , _lastChunkFinished :: Maybe (Frame, Count, Count, NominalDiffTime)
              , _lastFrameFinished :: Maybe Frame
              , _nodes :: M.Map String NodeState
              , _startTime :: Maybe UTCTime
              , _finishTime :: Maybe UTCTime
              , _totalTime :: Maybe NominalDiffTime
              , _imageSize :: Maybe (Width, Height)
              , _loadedMeshes :: Maybe Count
              , _meshesLoaded :: Bool
              , _setScene :: Bool
              , _started :: Bool
              , _finished :: Bool
              }

makeLenses ''St
makeLenses ''InfoState

theMap :: AttrMap
theMap = attrMap (white `on` black)
    [ (statusFinishedAttr,              fg green)
    , (statusStartedAttr,               fg yellow)
    , (nodeStateAttr Connecting,        fg red)
    , (nodeStateAttr Connected,         fg yellow)
    , (nodeStateAttr Ready,             fg green)
    , (hBorderLabelAttr,                fg cyan)
    ]

appEvent :: St -> AppEvent -> EventM (Next St)
appEvent st (VtyEvent (EvKey KEsc [])) = halt st
appEvent st (VtyEvent (EvKey (KChar 'q') [])) = halt st
appEvent st (Info i) =
    case i of
        ISampleRoot _ -> continue st
        ITraceMaxDepth _ -> continue st
        IConnected node -> continue $ st & infoState.nodes.at node .~ Just Connected
        IConnecting node -> continue $ st & infoState.nodes.at node .~ Just Connecting
        INodeReady node -> continue $ st & infoState.nodes.at node .~ Just Ready
        ISceneName name -> continue $ st & infoState.sceneName .~ Just name
        IFrameRange range -> continue $ st & infoState.frameRange .~ Just range
        INumObjects n -> continue $ st & infoState.numObjects .~ Just n
        IShadows s -> continue $ st & infoState.shadows .~ Just s
        INumCPUs n -> continue $ st & infoState.numCPUs .~ Just n
        IChunkFinished fn start stop total -> continue $ st &  infoState.lastChunkFinished .~ Just (fn, start, stop, total)
        IStartTime start -> continue $ st & infoState.startTime .~ Just start
        IFinishTime finish -> continue $ st & infoState.finishTime .~ Just finish
        ITotalTime total -> continue $ st & infoState.totalTime .~ Just total
        IImageSize w h -> continue $ st & infoState.imageSize .~ Just (w, h)
        ILoadedMeshes n -> continue $ st & infoState.loadedMeshes .~ Just n
                                         & infoState.meshesLoaded .~ True
        ILoadingMeshes -> continue $ st & infoState.meshesLoaded .~ False
        ISettingScene -> continue $ st & infoState.setScene .~ True
        IStarted -> continue $ st & infoState.started .~ True
        IFinished fn -> continue $ st & infoState.lastFrameFinished .~ Just fn
        IShutdown -> continue $ st & infoState.finished .~ True

appEvent st GUIShutdown = halt st
appEvent st _ = continue st

nodeStateAttr :: NodeState -> AttrName
nodeStateAttr Connecting = "nodeStateConnecting"
nodeStateAttr Connected = "nodeStateConnected"
nodeStateAttr Ready = "nodeStateReady"

statusFinishedAttr :: AttrName
statusFinishedAttr = "statusFinished"

statusStartedAttr :: AttrName
statusStartedAttr = "statusStarted"

drawUI :: St -> [Widget]
drawUI st = [withBorderStyle unicode ui]
    where
        ui = vBox [ hBorderWithLabel $ str "tracy"
                  , drawPreConfig $ st^.preConfig
                  , drawInfoState (st^.preConfig) (st^.infoState)
                  ]

mkNodeEntry :: (String, NodeState) -> Widget
mkNodeEntry (name, st) =
    (str $ take 25 name) <+>
    (padLeft Max (withAttr (nodeStateAttr st) $ str $ show st))

drawInfoState :: PreConfig -> InfoState -> Widget
drawInfoState pcfg st =
    let curFrameStatus = case st^.lastChunkFinished of
            Nothing -> 0.0
            Just (_, Count fin, Count total, _) -> toEnum fin / toEnum total
        curFrameName = case st^.lastChunkFinished of
            Nothing -> "-"
            Just (Frame fn, _, _, _) -> "Frame " <> show fn
        finishedFrameLabel = case st^.lastFrameFinished of
            Nothing -> case argFrameStop pcfg of
                Nothing -> "0/1"
                Just stop -> "0/" <> show stop
            Just (Frame cur) -> case argFrameStop pcfg of
                Nothing -> "1/1"
                Just stop -> (show $ cur - (argFrameStart pcfg) + 1) <> "/" <> show (stop - (argFrameStart pcfg) + 1)
        finishedFrames = case st^.lastFrameFinished of
            Nothing -> 0.0
            Just (Frame cur) -> case argFrameStop pcfg of
                Nothing -> 1.0
                Just stop -> (toEnum $ cur - (argFrameStart pcfg) + 1) / (toEnum $ stop - (argFrameStart pcfg))
        estTimeRemaining = case st^.lastChunkFinished of
            Nothing -> "-"
            Just (_, _, _, diffTime) ->
                let t = fromEnum diffTime `div` 1000000000000
                    h = t `div` 3600
                    m = (t `mod` 3600) `div` 60
                    s = (t `mod` 3600) `mod` 60
                    totalStr = show h <> "h " <> show m <> "m " <> show s <> "s"
                in totalStr
    in hBox [ vBox [ hBorderWithLabel $ str "Rendering Status"
                   , labeledValue "Status:" (if st^.finished
                                             then withAttr statusFinishedAttr $ str "finished"
                                             else if st^.started
                                                     then withAttr statusStartedAttr $ str "rendering"
                                                     else str "-")
                    , labeledValue "Scene name:" (str $ maybe "-" id $ st^.sceneName)
                    , labeledValue "Start time:" (mValue $ str <$> show <$> st^.startTime)
                    , labeledValue "Est. time remaining:" (str estTimeRemaining)
                    , labeledValue "Finish time:" (mValue $ str <$> show <$> st^.finishTime)
                    , labeledValue "Total time:" (mValue $ str <$> show <$> st^.totalTime)
                    , labeledValue "# objects:" (mValue $ str <$> show <$> fromEnum <$> st^.numObjects)
                    , labeledValue "Loaded meshes:" (mValue $ str <$> show <$> fromEnum <$> st^.loadedMeshes)
                    , labeledValue "Shadows:" (mValue $ str <$> show <$> st^.shadows)
                    , labeledValue "Image size:" (mValue $ str <$> show <$> st^.imageSize)
                    , labeledValue "Current frame status:" (updateAttrMap (applyAttrMappings [(progressCompleteAttr, white `on` blue)]) $
                                                            progressBar (Just curFrameName) curFrameStatus)
                    , labeledValue "Finished frames:" (updateAttrMap (applyAttrMappings [(progressCompleteAttr, black `on` yellow)]) $
                                                       progressBar (Just finishedFrameLabel) finishedFrames)
                    , fill ' '
                    ]
             , (str [bsIntersectT unicode]) <=> vBorder
             , hLimit 40 $ hBorderWithLabel (str "Nodes")
                           <=> (vBox $ mkNodeEntry <$> M.assocs (st^.nodes))
                           <=> fill ' '
             ]

mValue :: Maybe Widget -> Widget
mValue Nothing = str "-"
mValue (Just w) = w

labeledValue :: String -> Widget -> Widget
labeledValue label a = padRight Max $
                       (hLimit 25 $ padRight Max $ str label) <+>
                       (padRight Max a)

showValue :: (Show a) => String -> a -> Widget
showValue label a = padRight Max $
                    (hLimit 25 $ padRight Max $ str label) <+>
                    (padRight Max $ str $ show a)

drawPreConfig :: PreConfig -> Widget
drawPreConfig cfg =
    hBox [ vBox [ showValue "Sample root:" (argSampleRoot cfg)
                , showValue "CPU count:" (argCpuCount cfg)
                , showValue "Start frame:" (argFrameStart cfg)
                , showValue "Stop frame:" (argFrameStop cfg)
                , showValue "Force shadows:" (argForceShadows cfg)
                ]
         , vBox [ showValue "Samples per chunk:" (argSamplesPerChunk cfg)
                , showValue "Rows per chunk:" (argRowsPerChunk cfg)
                , showValue "Render mode:" (argRenderMode cfg)
                , showValue "Trace depth:" (argTraceMaxDepth cfg)
                , padRight Max $ str " "
                ]
         ]

app :: App St AppEvent
app =
    App { appDraw = drawUI
        , appChooseCursor = neverShowCursor
        , appStartEvent = return
        , appHandleEvent = appEvent
        , appLiftVtyEvent = VtyEvent
        , appAttrMap = const theMap
        }

brickHandler :: Chan InfoEvent -> Chan AppEvent -> IO ()
brickHandler infoChan appChan =
    forever $ do
        ev <- Info <$> readChan infoChan
        writeChan appChan ev

main :: IO ()
main = do
  args <- getArgs
  opts <- mkOpts
  let (os, rest, _) = getOpt Permute opts args

  defPreCfg <- defaultPreConfig
  preCfg <- foldM updateConfig defPreCfg os

  when (Help `elem` os) usage
  when (length rest /= 1) usage
  case argFrameStop preCfg of
      Nothing -> return ()
      Just f -> when (f < argFrameStart preCfg) $ usage

  let [toRender] = rest

  setNumCapabilities $ argCpuCount preCfg

  result <- loadSceneDesc toRender
  case result of
      Left e -> putStrLn ("Could not load scene: " ++ e) >> exitFailure
      Right sceneDesc -> do
        let renderCfg = defaultRenderConfig & sampleRoot .~ (argSampleRoot preCfg)
                                            & forceShadows .~ (argForceShadows preCfg)
                                            & samplesPerChunk .~ (argSamplesPerChunk preCfg)
                                            & rowsPerChunk .~ (Height $ argRowsPerChunk preCfg)
                                            & renderMode .~ (argRenderMode preCfg)

        iChan <- newChan
        dChan <- newChan
        appChan <- newChan

        -- Modify the scene description according to the command-line
        -- parameters
        let configuredSceneDesc = case argTraceMaxDepth preCfg of
              Nothing -> sceneDesc
              Just v -> sceneDesc & sceneDescWorld.wdViewPlane.vpMaxDepth .~ v

            (numNodes, manager) = if null $ argRenderNodes preCfg
                                  then (1, localRenderManager)
                                  else ( length $ argRenderNodes preCfg
                                       , networkRenderManager (argRenderNodes preCfg) iChan
                                       )
            cfgFrameRange = ( Frame $ argFrameStart preCfg
                         , case argFrameStop preCfg of
                             Nothing -> Frame $ argFrameStart preCfg
                             Just f -> Frame f
                         )

        let initialState = St renderCfg preCfg initInfoState
            initInfoState = InfoState { _sceneName = Nothing
                                      , _frameRange = Nothing
                                      , _numObjects = Nothing
                                      , _shadows = Nothing
                                      , _numCPUs = Nothing
                                      , _lastChunkFinished = Nothing
                                      , _lastFrameFinished = Nothing
                                      , _nodes = M.empty
                                      , _startTime = Nothing
                                      , _finishTime = Nothing
                                      , _totalTime = Nothing
                                      , _imageSize = Nothing
                                      , _loadedMeshes = Nothing
                                      , _meshesLoaded = False
                                      , _setScene = False
                                      , _started = False
                                      , _finished = False
                                      }

        _ <- forkIO $ brickHandler iChan appChan
        _ <- forkIO $ TM.render toRender renderCfg configuredSceneDesc cfgFrameRange numNodes manager iChan dChan

        case UseGUI `elem` os of
            False -> do
                void $ forkIO $ fileHandler dChan
                void $ customMain (mkVty def) appChan app initialState
            True -> do
                mv <- newEmptyMVar
                stopMVar <- newEmptyMVar
                void $ forkIO $ do
                    void $ customMain (mkVty def) appChan app initialState
                    putMVar stopMVar ()
                    putMVar mv ()

                glfwHandler stopMVar dChan
                writeChan appChan GUIShutdown
                void $ takeMVar mv
