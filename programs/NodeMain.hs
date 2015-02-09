module Main where

import Control.Concurrent
import Control.Monad
import System.Console.GetOpt
import System.Environment
import System.Exit
import GHC.Conc
import System.ZMQ4
import Data.Serialize (encode, decode)

import Tracy.Types
import Tracy.RenderManagers.Local

data Arg = Help
         | CPUs String
         | ListenPort String
           deriving (Eq, Show)

data PreConfig =
    PreConfig { argCpuCount :: Int
              , argListenPort :: Int
              }

defaultListenPort :: Int
defaultListenPort = 9000

defaultPreConfig :: IO PreConfig
defaultPreConfig = do
    n <- getNumProcessors
    return $ PreConfig { argCpuCount = n
                       , argListenPort = defaultListenPort
                       }

mkOpts :: IO [OptDescr Arg]
mkOpts = do
    maxc <- getNumProcessors
    return [ Option "h" ["help"] (NoArg Help) "This help output"
           , Option "p" ["port"] (ReqArg ListenPort "PORT")
             ("Port to listen on (default: " ++ show defaultListenPort ++ ")")
           , Option "c" ["cpu-count"] (ReqArg CPUs "COUNT")
             ("Number of CPUs to use (max: " ++ show maxc ++ ")")
           ]

updateConfig :: PreConfig -> Arg -> IO PreConfig
updateConfig c Help = return c
updateConfig c (ListenPort s) = do
    case reads s of
        [(p, _)] -> do
            when (p < 1 || p >= 65536) $ do
                putStrLn $ "Error: invalid port: " ++ show p
                exitFailure
            return $ c { argListenPort = p }
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
  let header = "Usage: " ++ pn ++ " [options]"
  putStrLn $ usageInfo header opts
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  opts <- mkOpts
  let (os, _, _) = getOpt Permute opts args

  defPreCfg <- defaultPreConfig
  preCfg <- foldM updateConfig defPreCfg os

  when (Help `elem` os) usage

  setNumCapabilities $ argCpuCount preCfg

  jobReq <- newChan
  jobResp <- newChan

  -- Start the local renderer in a loop, since each time a render is finished,
  -- it terminates
  _ <- forkIO $ forever $ localRenderManager jobReq jobResp

  withContext $ \ctx -> do
      sock <- socket ctx Rep
      bind sock $ "tcp://*:" ++ (show $ argListenPort preCfg)

      forever $ do
        msg <- receive sock
        case decode msg of
            Left e -> putStrLn $ "Error decoding message: " ++ e
            Right val -> do
                case val of
                    SetScene cfg _ _ _ _ ->
                        putStrLn $ "Got scene setting, cfg: " ++ show cfg
                    SetFrame (Frame fn) ->
                        putStrLn $ "Got frame setting for frame number " ++ show fn
                    RenderRequest _ _ -> putStrLn $ "Got render request"
                    FrameFinished -> putStrLn "Got frame finished"
                    RenderFinished -> putStrLn "Rendering finished"
                    Shutdown -> putStrLn "Shutdown"

                writeChan jobReq val
                resp <- readChan jobResp
                send sock [] $ encode $ snd resp

                case snd resp of
                    JobAck -> return ()
                    SetSceneAck -> putStrLn "Done setting up scene, ready for frames"
                    SetFrameAck -> putStrLn "Done setting up frame, ready to render"
                    ChunkFinished _ _ _ -> putStrLn $ "Finished chunk"
                    JobError e -> putStrLn $ "Job error: " ++ e
