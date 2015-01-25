{-# LANGUAGE ScopedTypeVariables #-}
module Tracy.SceneLoader
  ( loadScene
  )
  where

import qualified Data.Yaml as Y
import qualified Data.ByteString.Lazy.Char8 as BS8
import Data.Aeson.Encode.Pretty (encodePretty)

import Tracy.Types

loadScene :: FilePath -> IO ()
loadScene path = do
    result <- Y.decodeFileEither path
    case result of
        Left e -> print e
        Right (v::Y.Object) -> putStrLn $ BS8.unpack $ encodePretty v
