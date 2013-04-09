module Tracy.Main where

import Control.Lens
import Control.Monad
import Control.Monad.State
import Codec.BMP
import Data.Time.Clock

import Tracy.World

data Config =
    Config { showLog :: Bool
           , silent :: Bool
           }
    deriving (Eq, Show)

defaultConfig :: Config
defaultConfig =
    Config { showLog = False
           , silent = False
           }

render :: Config -> World -> FilePath -> IO ()
render cfg w filename = do
  let putMsg = when (not $ silent cfg) . putStrLn
      putLog = when (showLog cfg) . putStrLn

  putMsg $ "Rendering " ++ filename ++ " ..."
  putMsg $ "  Objects: " ++ (w^.objects^.to length^.to show)

  t1 <- getCurrentTime
  let (img, st) = runState (renderWorld w) (TraceState [])
  writeBMP filename img

  when (length (st^.traceLog) > 0) $
       do
         putLog "  Log:"
         forM_ (st^.traceLog.to reverse) $ \m ->
             putLog $ "    " ++ m

  t2 <- getCurrentTime

  putMsg $ "done. Total time: " ++ (show $ diffUTCTime t2 t1)
