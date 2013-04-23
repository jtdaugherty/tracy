module Tracy.Main where

import Control.Lens
import Control.Monad
import Control.Monad.State
import System.Random
import Codec.BMP
import Data.Time.Clock

import Tracy.Types
import Tracy.Samplers

defaultConfig :: Config
defaultConfig =
    Config { showLog = False
           , silent = False
           , vpSampler = regular
           , sampleRoot = 4
           }

render :: Config -> World -> TraceM BMP -> FilePath -> IO ()
render cfg w tracer filename = do
  let putMsg = when (not $ silent cfg) . putStrLn
      putLog = when (showLog cfg) . putStrLn

  putMsg $ "Rendering " ++ filename ++ " ..."
  putMsg $ "  Objects: " ++ (w^.objects.to length.to show)

  t1 <- getCurrentTime
  g <- getStdGen
  let (img, stNew) = runState tracer st
      st = TraceState { _traceLog = []
                      , _traceRNG = g
                      , _traceConfig = cfg
                      -- XXX: this is going to cause artifacts
                      -- later. We need the number of sets to be
                      -- relatively prime to the number of columns.  I
                      -- picked the hres here to force aliasing
                      -- artifacts so I'll fix this later.
                      , _traceNumSampleSets = w^.viewPlane.hres.to (*1.3).from enum
                      }

  writeBMP filename img

  when (length (stNew^.traceLog) > 0) $
       do
         putLog "  Log:"
         forM_ (stNew^.traceLog.to reverse) $ \m ->
             putLog $ "    " ++ m

  t2 <- getCurrentTime

  putMsg $ "done. Total time: " ++ (show $ diffUTCTime t2 t1)
