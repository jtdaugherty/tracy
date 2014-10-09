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
import System.Random (StdGen, setStdGen)

import Tracy.Types
import Tracy.SceneBuilder
import Tracy.Samplers
import Tracy.ChunkRender

localSetSceneAndRender :: Chan JobRequest -> Chan JobResponse -> RenderConfig -> Scene ThinLens -> StdGen -> IO ()
localSetSceneAndRender jobReq jobResp cfg builtScene gen = do
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

    -- Set the global random number generator so we don't have bias in
    -- our samples relative to other nodes
    setStdGen gen

    -- Generate sample data for square and disk samplers
    sSamples <- replicateM numSets $ squareSampler (cfg^.sampleRoot)
    dSamples <- replicateM numSets $ diskSampler (cfg^.sampleRoot)

    let sSamplesVec = V.fromList sSamples
        dSamplesVec = V.fromList dSamples

    let processRequests = do
          ev <- readChan jobReq
          case ev of
              RenderRequest chunkId (start, stop) -> do
                  ch <- renderChunk cfg scene (start, stop) tracer sSamplesVec dSamplesVec sSamplesVec
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
              SetScene cfg sDesc gen -> do
                  case sceneFromDesc sDesc of
                      Right s -> do
                          writeChan jobResp JobAck
                          localSetSceneAndRender jobReq jobResp cfg s gen
                      Left e -> writeChan jobResp $ JobError e
                  waitForJob
              Shutdown -> do
                  writeChan jobResp JobAck
              RenderFinished -> writeChan jobResp JobAck
              _ -> writeChan jobResp $ JobError "Expected SetScene or Shutdown, got unexpected event"

    waitForJob
