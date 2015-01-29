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
    let sqSampler = builtScene^.sceneWorld.viewPlane.squareSampler
        diskSampler = builtScene^.sceneCamera.cameraData.lensSampler
        theNumSets = fromEnum $ 10 * builtScene^.sceneWorld.viewPlane.hres
        aScheme = builtScene^.sceneAccelScheme
        worldAccel = (aScheme^.schemeApply) (builtScene^.sceneWorld)
        worldAccelShadows = case cfg^.forceShadows of
                              Nothing -> worldAccel
                              Just v -> worldAccel & worldShadows .~ v
        scene = builtScene & sceneWorld .~ worldAccelShadows
        tracer = builtScene^.sceneTracer

    let processRequests = do
          ev <- readChan jobReq
          case ev of
              RenderRequest -> do
                  -- Generate sample data for square and disk samplers
                  sSamplesVec <- V.generateM theNumSets $ const $ runSampler sqSampler rng (cfg^.sampleRoot)
                  dSamplesVec <- V.generateM theNumSets $ const $ runSampler diskSampler rng (cfg^.sampleRoot)
                  oSamplesVec <- V.generateM theNumSets $ const $ runSampler sqSampler rng (cfg^.sampleRoot)

                  let sampleData = SampleData theNumSets sSamplesVec dSamplesVec oSamplesVec

                  ch <- renderChunk cfg rng scene tracer sampleData
                  writeChan jobResp $ BatchFinished ch
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
              SetScene cfg sDesc mg fn -> do
                  case sceneFromDesc sDesc mg fn of
                      Right s -> do
                          writeChan jobResp JobAck
                          gen <- createSystemRandom
                          localSetSceneAndRender jobReq jobResp cfg s gen
                      Left e -> writeChan jobResp $ JobError e
                  waitForJob
              Shutdown -> do
                  writeChan jobResp JobAck
              RenderFinished -> writeChan jobResp JobAck
              _ -> writeChan jobResp $ JobError "Expected SetScene or Shutdown, got unexpected event"

    waitForJob
