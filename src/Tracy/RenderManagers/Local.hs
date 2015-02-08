{-# LANGUAGE BangPatterns #-}
module Tracy.RenderManagers.Local
  ( localRenderManager
  )
  where

import Control.Applicative
import Control.Concurrent.Chan
import Control.Lens
import Control.Monad
import qualified Data.Map as M
import qualified Data.Vector as V
import System.Random.MWC

import Tracy.Types
import Tracy.SceneBuilder
import Tracy.Samplers
import Tracy.ChunkRender

localNodeName :: String
localNodeName = "<in-process renderer>"

localSetSceneAndRender :: Chan JobRequest -> Chan (String, JobResponse) -> RenderConfig
                       -> Scene ThinLens -> SampleData -> M.Map Int [V.Vector Int]
                       -> IO ()
localSetSceneAndRender jobReq jobResp cfg builtScene sampleData sampleIndexMap = do
    let aScheme = builtScene^.sceneAccelScheme
        worldAccel = (aScheme^.schemeApply) (builtScene^.sceneWorld)
        worldAccelShadows = case cfg^.forceShadows of
                              Nothing -> worldAccel
                              Just v -> worldAccel & worldShadows .~ v
        scene = builtScene & sceneWorld .~ worldAccelShadows
        tracer = builtScene^.sceneTracer

        processRequests = do
          ev <- readChan jobReq
          case ev of
              RenderRequest rowRange sampleRange -> do
                  let Row startRow = fst rowRange
                      !sampleIndices = sampleIndexMap M.! startRow
                  ch <- renderChunk cfg scene tracer sampleData sampleIndices sampleRange rowRange
                  writeChan jobResp (localNodeName, ChunkFinished rowRange ch)
                  processRequests
              RenderFinished -> do
                  writeChan jobResp (localNodeName, JobAck)
              _ -> writeChan jobResp ( localNodeName
                                     , JobError "Expected RenderRequest or RenderFinished, got unexpected event"
                                     )

    processRequests

localRenderManager :: Chan JobRequest -> Chan (String, JobResponse) -> IO ()
localRenderManager jobReq jobResp = do
    let waitForJob = do
          reqEv <- readChan jobReq
          case reqEv of
              SetScene cfg sDesc mg fn seedV rowRanges -> do
                  case sceneFromDesc sDesc mg fn of
                      Right s -> do
                          let pxSampler = s^.sceneWorld.viewPlane.pixelSampler
                              sqSampler = correlatedMultiJittered
                              diskSampler = s^.sceneCamera.cameraData.lensSampler
                              theNumSets = fromEnum $ s^.sceneWorld.viewPlane.hres
                              seed = toSeed seedV

                          rng <- restore seed

                          -- Generate sample data for square and disk samplers
                          pSamplesVec <- V.generateM theNumSets $ const $ runSampler pxSampler rng (cfg^.sampleRoot)
                          sSamplesVec <- V.generateM theNumSets $ const $ runSampler sqSampler rng (cfg^.sampleRoot)
                          dSamplesVec <- V.generateM theNumSets $ const $ runSampler diskSampler rng (cfg^.sampleRoot)
                          oSamplesVec <- V.generateM theNumSets $ const $ runSampler sqSampler rng (cfg^.sampleRoot)

                          let sampleData = SampleData theNumSets pSamplesVec sSamplesVec dSamplesVec oSamplesVec

                          sampleIndexMap <- M.fromList <$>
                                            (forM rowRanges $ \(Row startRow, Row endRow) ->
                                                (,) <$> (pure startRow) <*>
                                                  (replicateM (endRow-startRow+1) $
                                                  V.replicateM (fromEnum $ s^.sceneWorld.viewPlane.hres) $
                                                    uniformR (0, sampleData^.numSets - 1) rng
                                                    )
                                                    )
                          writeChan jobResp (localNodeName, SetSceneAck)

                          localSetSceneAndRender jobReq jobResp cfg s sampleData sampleIndexMap
                      Left e -> writeChan jobResp (localNodeName, JobError e)
                  waitForJob
              Shutdown -> do
                  writeChan jobResp (localNodeName, JobAck)
              RenderFinished -> writeChan jobResp (localNodeName, JobAck)
              _ -> writeChan jobResp (localNodeName, JobError "Expected SetScene or Shutdown, got unexpected event")

    waitForJob
