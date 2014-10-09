module Tracy.RenderManagers.Local
  ( localRenderManager
  )
  where

import Control.Concurrent.Chan
import Control.Lens
import qualified Data.Vector as V
import System.Random.MWC

import Tracy.Types
import Tracy.SceneBuilder
import Tracy.Samplers
import Tracy.ChunkRender

localSetSceneAndRender :: Chan JobRequest -> Chan JobResponse -> RenderConfig -> Scene ThinLens -> GenIO -> IO ()
localSetSceneAndRender jobReq jobResp cfg builtScene rng = do
    let squareSampler = jittered
        diskSampler = builtScene^.sceneCamera.cameraData.lensSampler
        numSets = fromEnum $ builtScene^.sceneWorld.viewPlane.hres
        aScheme = builtScene^.sceneAccelScheme
        worldAccel = (aScheme^.schemeApply) (builtScene^.sceneWorld)
        worldAccelShadows = case cfg^.forceShadows of
                              Nothing -> worldAccel
                              Just v -> worldAccel & worldShadows .~ v
        scene = builtScene & sceneWorld .~ worldAccelShadows
        tracer = builtScene^.sceneTracer

    -- Generate sample data for square and disk samplers
    sSamplesVec <- V.generateM numSets $ const $ squareSampler rng (cfg^.sampleRoot)
    dSamplesVec <- V.generateM numSets $ const $ diskSampler rng (cfg^.sampleRoot)

    let processRequests = do
          ev <- readChan jobReq
          case ev of
              RenderRequest chunkId (start, stop) -> do
                  ch <- renderChunk cfg rng scene (start, stop) tracer sSamplesVec dSamplesVec sSamplesVec
                  writeChan jobResp $ ChunkFinished chunkId (start, stop) ch
                  processRequests
              RenderFinished -> do
                  writeChan jobResp JobAck
              _ -> writeChan jobResp $ JobError "Expected RenderRequest or RenderFinished, got unexpected event"

    processRequests

localRenderManager :: Chan JobRequest -> Chan JobResponse -> IO ()
localRenderManager jobReq jobResp = do
    let waitForJob = do
          reqEv <- readChan jobReq
          case reqEv of
              SetScene cfg sDesc seed -> do
                  case sceneFromDesc sDesc of
                      Right s -> do
                          writeChan jobResp JobAck
                          gen <- restore seed
                          localSetSceneAndRender jobReq jobResp cfg s gen
                      Left e -> writeChan jobResp $ JobError e
                  waitForJob
              Shutdown -> do
                  writeChan jobResp JobAck
              RenderFinished -> writeChan jobResp JobAck
              _ -> writeChan jobResp $ JobError "Expected SetScene or Shutdown, got unexpected event"

    waitForJob
