module Main where

import Control.Applicative
import Control.Monad
import Control.Lens
import Data.List (intercalate)
import System.Console.GetOpt
import System.Environment
import System.Exit
import GHC.Conc

import Tracy.Main
import Tracy.Scenes
import Tracy.Types

data Arg = Help
         | SampleRoot String
         | NoShadows
         | Shadows
         | SchemeArg String
         | CPUs String
           deriving (Eq, Show)

schemes :: [(String, AccelScheme)]
schemes =
    [ ("none", AccelNone)
    , ("grid", AccelGrid)
    ]

mkOpts :: Int -> IO [OptDescr Arg]
mkOpts maxc =
    return [ Option "h" ["help"] (NoArg Help) "This help output"
           , Option "r" ["aa-sample-root"] (ReqArg SampleRoot "ROOT") "AA sample root"
           , Option "n" ["force-no-shadows"] (NoArg NoShadows) "Force shadows off"
           , Option "s" ["force-shadows"] (NoArg Shadows) "Force shadows on"
           , Option "a" ["accel"] (ReqArg SchemeArg "SCHEME")
           ("Acceleration scheme\nValid options:\n " ++ intercalate "\n " (fst <$> schemes))
           , Option "c" ["cpu-count"] (ReqArg CPUs "COUNT")
           ("Number of CPUs to use (max: " ++ show maxc ++ ")")
           ]

updateConfig :: Config -> Arg -> IO Config
updateConfig c Help = return c
updateConfig c (SampleRoot s) = return $ c { sampleRoot = read s }
updateConfig c NoShadows = return c
updateConfig c Shadows = return c
updateConfig c (CPUs s) = do
    case reads s of
        [(cnt, _)] -> do
            avail <- getNumProcessors
            when (cnt < 1 || cnt > avail) $ do
                putStrLn $ concat [ "Error:\n"
                                  , "Requested CPUs: " ++ show cnt
                                  , "\nAvailable: " ++ show avail
                                  ]
                exitFailure
            return $ c { cpuCount = cnt }
        _ -> usage >> exitFailure
updateConfig c (SchemeArg s) = do
    case lookup s schemes of
        Nothing -> usage >> exitFailure
        Just v -> return $ c { accelScheme = v }

usage :: IO ()
usage = do
  pn <- getProgName
  opts <- mkOpts =<< getNumProcessors
  let header = "Usage: " ++ pn ++ " [options] [scene name]"
  putStrLn $ usageInfo header opts
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  opts <- mkOpts =<< getNumProcessors
  let (os, rest, _) = getOpt Permute opts args

  defCfg <- defaultConfig
  cfg <- foldM updateConfig defCfg os

  when (Help `elem` os) usage

  let forceShadows = if Shadows `elem` os
                     then Just True
                     else if NoShadows `elem` os
                          then Just False
                          else Nothing
      toRender = if null rest
                 then fst <$> scenes
                 else rest

  setNumCapabilities $ cpuCount cfg

  forM_ toRender $ \n -> do
         case lookup n scenes of
           Nothing -> putStrLn $ "No such scene: " ++ n
           Just (c, w) -> do
               let w1 = applyAccelScheme (accelScheme cfg) w
                   w2 = case forceShadows of
                          Nothing -> w1
                          Just v -> w1 & worldShadows .~ v
               render cfg c w2 $ n ++ ".bmp"
