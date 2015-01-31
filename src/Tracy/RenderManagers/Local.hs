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

localSetSceneAndRender :: Chan JobRequest -> Chan JobResponse -> RenderConfig
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

    let processRequests = do
          ev <- readChan jobReq
          case ev of
              RenderRequest rowRange sampleRange -> do
                  let !sampleIndices = sampleIndexMap M.! (fst rowRange)
                  ch <- renderChunk cfg scene tracer sampleData sampleIndices sampleRange rowRange
                  writeChan jobResp $ BatchFinished rowRange ch
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
              SetScene cfg sDesc mg fn seedV rowRanges -> do
                  case sceneFromDesc sDesc mg fn of
                      Right s -> do
                          let pxSampler = s^.sceneWorld.viewPlane.pixelSampler
                              sqSampler = correlatedMultiJittered
                              diskSampler = s^.sceneCamera.cameraData.lensSampler
                              theNumSets = fromEnum $ 10 * s^.sceneWorld.viewPlane.hres
                              seed = toSeed seedV

                          writeChan jobResp JobAck
                          rng <- restore seed

                          -- Generate sample data for square and disk samplers
                          pSamplesVec <- V.generateM theNumSets $ const $ runSampler pxSampler rng (cfg^.sampleRoot)
                          sSamplesVec <- V.generateM theNumSets $ const $ runSampler sqSampler rng (cfg^.sampleRoot)
                          dSamplesVec <- V.generateM theNumSets $ const $ runSampler diskSampler rng (cfg^.sampleRoot)
                          oSamplesVec <- V.generateM theNumSets $ const $ runSampler sqSampler rng (cfg^.sampleRoot)

                          let sampleData = SampleData theNumSets pSamplesVec sSamplesVec dSamplesVec oSamplesVec

                          sampleIndexMap <- M.fromList <$>
                                            (forM rowRanges $ \(startRow, endRow) ->
                                                (,) <$> (pure startRow) <*>
                                                  (replicateM (endRow-startRow+1) $
                                                  V.replicateM (fromEnum $ s^.sceneWorld.viewPlane.hres) $
                                                    uniformR (0, sampleData^.numSets - 1) rng
                                                    )
                                                    )

                          localSetSceneAndRender jobReq jobResp cfg s sampleData sampleIndexMap
                      Left e -> writeChan jobResp $ JobError e
                  waitForJob
              Shutdown -> do
                  writeChan jobResp JobAck
              RenderFinished -> writeChan jobResp JobAck
              _ -> writeChan jobResp $ JobError "Expected SetScene or Shutdown, got unexpected event"

    waitForJob
