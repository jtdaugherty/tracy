{-# LANGUAGE ScopedTypeVariables #-}
module Tracy.SceneLoader
  ( loadScene
  )
  where

import qualified Data.Yaml as Y

import Tracy.Types

loadScene :: FilePath -> IO ()
loadScene path = do
    result <- Y.decodeFileEither path
    case result of
        Left e -> print e
        Right (v::SceneDesc) -> print v
