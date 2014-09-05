module Tracy.RenderManagers.Local
  ( localRenderManager
  )
  where

import Control.Applicative
import Control.Concurrent.Chan
import Control.Lens
import Control.Monad
import Data.Colour
import qualified Data.Vector as V

import Tracy.Types
import Tracy.SceneBuilder
import Tracy.Samplers
import Tracy.ChunkRender

localSetSceneAndRender :: Chan JobRequest -> Chan JobResponse -> RenderConfig -> SceneDesc -> IO ()
localSetSceneAndRender jobReq jobResp cfg sDesc = do
    let Right builtScene = sceneFromDesc sDesc
        squareSampler = regular
        diskSampler = builtScene^.sceneCamera.cameraData.lensSampler
        numSets = fromEnum $ sDesc^.sceneDescWorld^.wdViewPlane.hres
        aScheme = builtScene^.sceneAccelScheme
        worldAccel = (aScheme^.schemeApply) (builtScene^.sceneWorld)
        worldAccelShadows = case cfg^.forceShadows of
                              Nothing -> worldAccel
                              Just v -> worldAccel & worldShadows .~ v
        scene = builtScene & sceneWorld .~ worldAccelShadows

    -- Generate sample data for square and disk samplers
    sSamples <- replicateM numSets $ squareSampler (cfg^.sampleRoot)
    dSamples <- replicateM numSets $ diskSampler (cfg^.sampleRoot)

    let sSamplesVec = V.fromList sSamples
        dSamplesVec = V.fromList dSamples

    let processRequests = do
          ev <- readChan jobReq
          case ev of
              RenderRequest chunkId (start, stop) -> do
                  ch <- renderChunk cfg scene (start, stop) sSamplesVec dSamplesVec
                  let converted = (cdemote <$>) <$> ch
                  writeChan jobResp $ ChunkFinished chunkId (start, stop) converted
                  processRequests
              RenderFinished -> do
                  writeChan jobResp JobAck
              _ -> writeChan jobResp $ JobError "Expected RenderRequest or RenderFinished, got unexpected event"

    writeChan jobResp JobAck
    processRequests

localRenderManager :: Chan JobRequest -> Chan JobResponse -> IO ()
localRenderManager jobReq jobResp = do
    let waitForJob = do
          reqEv <- readChan jobReq
          case reqEv of
              SetScene cfg sDesc -> do
                  localSetSceneAndRender jobReq jobResp cfg sDesc
                  waitForJob
              Shutdown -> do
                  writeChan jobResp JobAck
              _ -> writeChan jobResp $ JobError "Expected SetScene or Shutdown, got unexpected event"

    waitForJob

