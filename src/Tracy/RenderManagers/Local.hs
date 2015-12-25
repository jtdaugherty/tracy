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
localNodeName = "<local>"

localSetSceneAndRender :: Chan JobRequest -> Chan (String, JobResponse) -> RenderConfig
                       -> SceneDesc -> ImageGroup -> MeshGroup -> SampleData -> M.Map Int [V.Vector Int]
                       -> IO ()
localSetSceneAndRender jobReq jobResp cfg sDesc ig mg sampleData sampleIndexMap = do
    let processRequests builtScene = do
          let baseWorld = builtScene^.sceneWorld
              shadowWorld = case cfg^.forceShadows of
                               Nothing -> baseWorld
                               Just v -> baseWorld & worldShadows .~ v
              scene = builtScene & sceneWorld .~ shadowWorld
              tracer = builtScene^.sceneTracer

          ev <- readChan jobReq
          case ev of
              RenderRequest rowRange sampleRange@(sa, sb) -> do
                  let Row startRow = fst rowRange
                      !sampleIndices = sampleIndexMap M.! startRow
                      sc = length [sa..sb]
                  ch <- renderChunk cfg scene tracer sampleData sampleIndices sampleRange rowRange
                  writeChan jobResp (localNodeName, ChunkFinished rowRange (Count sc) ch)
                  processRequests builtScene
              FrameFinished -> writeChan jobResp (localNodeName, JobAck) >> return True
              RenderFinished -> writeChan jobResp (localNodeName, JobAck) >> return False
              _ -> do
                  writeChan jobResp ( localNodeName
                                    , JobError "Expected RenderRequest or RenderFinished, got unexpected event"
                                    )
                  return False

        processFrames = do
          ev <- readChan jobReq
          case ev of
              SetFrame fn -> do
                case sceneFromDesc sDesc ig mg fn of
                    Right s -> do
                        writeChan jobResp (localNodeName, SetFrameAck)
                        continue <- processRequests s
                        when continue processFrames
                    Left e -> writeChan jobResp ( localNodeName
                                                , JobError $ "Could not create scene from description for frame " ++ (show fn) ++ ": " ++ e
                                                )
              RenderFinished -> writeChan jobResp (localNodeName, JobAck)
              _ -> writeChan jobResp ( localNodeName
                                     , JobError "Unexpected request; expected SetFrame!"
                                     )

    processFrames

localRenderManager :: Chan JobRequest -> Chan (String, JobResponse) -> IO ()
localRenderManager jobReq jobResp = do
    let waitForJob = do
          reqEv <- readChan jobReq
          case reqEv of
              SetScene cfg sDesc ig mg seedV rowRanges -> do
                  -- NOTE: this creates a "bogus" scene for frame 1 even
                  -- though we won't use this frame (necessarily). This
                  -- is just so we can get access to the samplers and
                  -- other details that will not (or should not) change
                  -- per frame.
                  case sceneFromDesc sDesc ig mg (Frame 1) of
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

                          localSetSceneAndRender jobReq jobResp cfg sDesc ig mg sampleData sampleIndexMap
                      Left e -> writeChan jobResp (localNodeName, JobError e)
                  waitForJob
              Shutdown -> do
                  writeChan jobResp (localNodeName, JobAck)
              RenderFinished -> writeChan jobResp (localNodeName, JobAck)
              FrameFinished -> writeChan jobResp (localNodeName, JobError "Expected SetScene or Shutdown, got FrameFinished")
              SetFrame _ -> writeChan jobResp (localNodeName, JobError "Expected SetScene or Shutdown, got SetFrame")
              _ -> writeChan jobResp (localNodeName, JobError "Expected SetScene or Shutdown, got unexpected event")

    waitForJob
