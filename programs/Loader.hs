module Main where

import System.Environment

import Tracy.SceneLoader

main :: IO ()
main = do
    [path] <- getArgs
    loadScene path
