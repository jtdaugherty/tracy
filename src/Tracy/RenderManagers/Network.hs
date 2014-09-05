module Tracy.RenderManagers.Network
  ( networkRenderManager
  )
  where

import Control.Concurrent
import Control.Monad
import Data.Serialize
import System.ZMQ4

import Tracy.Types

networkNodeThread :: String -> Chan InfoEvent -> Chan JobRequest -> Chan JobResponse -> IO () -> IO ()
networkNodeThread connStr iChan jobReq jobResp readyNotify = withContext $ \ctx -> do
    writeChan iChan $ IConnecting connStr
    sock <- socket ctx Req
    connect sock connStr
    writeChan iChan $ IConnected connStr

    readyNotify

    let worker = do
          ev <- readChan jobReq
          case ev of
              SetScene cfg s -> do
                  send sock [] $ encode $ SetScene cfg s
                  _ <- receive sock
                  worker
              RenderRequest chunkId (start, stop) -> do
                  _ <- send sock [] $ encode $ RenderRequest chunkId (start, stop)
                  reply <- receive sock
                  case decode reply of
                      Left e -> writeChan jobResp $ JobError e
                      Right r -> writeChan jobResp r
                  readyNotify
                  worker
              RenderFinished -> do
                  send sock [] $ encode RenderFinished
                  _ <- receive sock
                  worker
              Shutdown -> do
                  send sock [] $ encode Shutdown
                  _ <- receive sock
                  return ()

    worker

networkRenderManager :: [String] -> Chan InfoEvent -> Chan JobRequest -> Chan JobResponse -> IO ()
networkRenderManager nodes iChan jobReq jobResp = do
    reqChans <- replicateM (length nodes) newChan
    readyChan <- newChan

    -- Connect to all nodes
    forM_ (zip3 nodes reqChans [0..]) $ \(n, ch, i) -> do
        forkIO $ networkNodeThread n iChan ch jobResp (writeChan readyChan i)

    let sendToAll val = forM_ reqChans $ \ch -> writeChan ch val
        chanReader = do
            req <- readChan jobReq
            case req of
                SetScene cfg s -> do
                    sendToAll $ SetScene cfg s
                    chanReader
                RenderRequest chunkId (start, stop) -> do
                    -- Find available (non-busy) node
                    nodeId <- readChan readyChan
                    -- Send the request to its channel
                    writeChan (reqChans !! nodeId) $ RenderRequest chunkId (start, stop)
                    chanReader
                RenderFinished -> do
                    sendToAll RenderFinished
                    chanReader
                Shutdown -> do
                    sendToAll Shutdown

    chanReader
