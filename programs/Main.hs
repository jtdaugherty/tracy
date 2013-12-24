module Main where

import Control.Applicative
import Control.Monad
import Control.Lens
import Data.Maybe
import Data.List (intercalate)
import System.Console.GetOpt
import System.Environment
import System.Exit
import GHC.Conc

import Tracy.Main
import Tracy.Scenes
import Tracy.Types
import Tracy.Grid

data Arg = Help
         | SampleRoot String
         | NoShadows
         | SchemeArg String
           deriving (Eq, Show)

schemes :: [(String, AccelScheme)]
schemes =
    [ ("none", AccelNone)
    , ("grid", AccelGrid)
    ]

opts :: [OptDescr Arg]
opts = [ Option "h" ["help"] (NoArg Help) "This help output"
       , Option "a" ["aa-sample-root"] (ReqArg SampleRoot "ROOT") "AA sample root"
       , Option "n" ["no-shadows"] (NoArg NoShadows) "Turn off shadows"
       , Option "s" ["scheme"] (ReqArg SchemeArg "SCHEME")
         ("Acceleration scheme (options: " ++ intercalate ", " (fst <$> schemes) ++ ")")
       ]

updateConfig :: Config -> Arg -> IO Config
updateConfig c Help = return c
updateConfig c (SampleRoot s) = return $ c { sampleRoot = read s }
updateConfig c NoShadows = return $ c { shadows = False }
updateConfig c (SchemeArg s) = do
    case lookup s schemes of
        Nothing -> usage >> exitFailure
        Just v -> return $ c { accelScheme = v }

usage :: IO ()
usage = do
  pn <- getProgName
  let header = "Usage: " ++ pn ++ " [options] [scene name]"
  putStrLn $ usageInfo header opts
  exitFailure

main :: IO ()
main = do
  setNumCapabilities =<< getNumProcessors

  args <- getArgs
  let (os, rest, _) = getOpt Permute opts args

  cfg <- foldM updateConfig defaultConfig os

  when (Help `elem` os) usage

  let toRender = if null rest
                 then fst <$> scenes
                 else rest

  forM_ toRender $ \n -> do
         case lookup n scenes of
           Nothing -> putStrLn $ "No such scene: " ++ n
           Just (c, w) -> do
               let accelWorld = case accelScheme cfg of
                                  AccelNone -> w
                                  AccelGrid -> let gObjs = [o | o <- _objects w, isJust $ o^.bounding_box ]
                                                   objs = [o | o <- _objects w, not $ isJust $ o^.bounding_box ]
                                               in w { _objects = grid gObjs:objs }
               render cfg c accelWorld $ n ++ ".bmp"
