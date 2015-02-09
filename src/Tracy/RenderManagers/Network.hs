module Tracy.RenderManagers.Network
  ( networkRenderManager
  )
  where

import Control.Concurrent
import Control.Monad
import Data.Serialize
import System.ZMQ4

import Tracy.Types

networkNodeThread :: String -> Chan InfoEvent -> Chan JobRequest -> Chan (String, JobResponse) -> IO () -> IO ()
networkNodeThread connStr iChan jobReq jobResp readyNotify = withContext $ \ctx -> do
    writeChan iChan $ IConnecting connStr
    withSocket ctx Req $ \sock -> do
      connect sock connStr
      writeChan iChan $ IConnected connStr

      let worker = do
            ev <- readChan jobReq
            send sock [] $ encode ev
            reply <- receive sock
            case decode reply of
                Left e -> writeChan jobResp (connStr, JobError e)
                Right r -> writeChan jobResp (connStr, r)
            case ev of
                SetScene _ _ _ _ _ -> readyNotify >> worker
                SetFrame _ -> worker
                RenderRequest _ _ -> readyNotify >> worker
                RenderFinished -> worker
                FrameFinished -> worker
                Shutdown -> return ()

      worker

networkRenderManager :: [String] -> Chan InfoEvent -> Chan JobRequest -> Chan (String, JobResponse) -> IO ()
networkRenderManager nodes iChan jobReq jobResp = do
    reqChans <- replicateM (length nodes) newChan
    readyChan <- newChan

    -- Connect to all nodes
    forM_ (zip3 nodes reqChans [0..]) $ \(n, ch, i) -> do
        forkOS $ networkNodeThread n iChan ch jobResp (writeChan readyChan i)

    let sendToAll val = forM_ reqChans $ \ch -> writeChan ch val
        chanReader = do
            req <- readChan jobReq
            case req of
                SetScene _ _ _ _ _ -> do
                    sendToAll req
                    chanReader
                SetFrame _ -> do
                    sendToAll req
                    chanReader
                RenderRequest _ _ -> do
                    -- Find available (non-busy) node
                    nodeId <- readChan readyChan
                    -- Send the request to its channel
                    writeChan (reqChans !! nodeId) req
                    chanReader
                FrameFinished -> do
                    sendToAll FrameFinished
                    chanReader
                RenderFinished -> do
                    sendToAll RenderFinished
                    chanReader
                Shutdown -> do
                    sendToAll Shutdown

    -- Cancel any existing job
    sendToAll RenderFinished

    chanReader
