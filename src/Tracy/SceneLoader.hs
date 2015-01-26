{-# LANGUAGE ScopedTypeVariables #-}
module Tracy.SceneLoader
  ( loadSceneDesc
  )
  where

import qualified Data.Yaml as Y

import Tracy.Types

loadSceneDesc :: FilePath -> IO (Either String SceneDesc)
loadSceneDesc path = do
    result <- Y.decodeFileEither path
    case result of
        Left e -> return $ Left $ show e
        Right (v::SceneDesc) -> return $ Right v
