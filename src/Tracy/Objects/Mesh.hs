module Tracy.Objects.Mesh
  ( mesh
  , loadMesh
  ) where

import Control.Applicative
import Control.Concurrent
import Data.ByteString.Char8 hiding (putStrLn)
import qualified Data.Vector as V
import Linear
import PLY
import PLY.Types
import GHC.Float

import Tracy.Types
import Tracy.Objects.Grid
import Tracy.Objects.Triangle

loadMesh :: FilePath -> IO MeshData
loadMesh filename = do
    putStrLn $ "Loading mesh from " ++ filename ++ "..."

    mv1 <- newEmptyMVar
    mv2 <- newEmptyMVar

    _ <- forkIO $ do
      Right vs <- loadElements (pack "vertex") filename
      putMVar mv1 vs

    _ <- forkIO $ do
      Right fs <- loadElements (pack "face") filename
      putMVar mv2 fs

    vs <- takeMVar mv1
    putStrLn $ "  " ++ show (V.length vs) ++ " vertices"
    fs <- takeMVar mv2
    putStrLn $ "  " ++ show (V.length fs) ++ " faces"

    let toDouble (Sfloat f) = float2Double f
        toDouble e = error $ "Could not get float from scalar: " ++ show e
        toInt (Suint i) = fromEnum i
        toInt (Sint i) = i
        toInt e = error $ "Could not get int from scalar: " ++ show e

        vVecs = V.map mkVec vs
        mkVec vals = ( V3 (toDouble $ vals V.! 0) (toDouble $ vals V.! 1) (toDouble $ vals V.! 2)
                     , V3 (toDouble $ vals V.! 3) (toDouble $ vals V.! 4) (toDouble $ vals V.! 5)
                     )

        -- Vector (Vector Scalar) -> Vector [Int] -> [[Int]]
        intFs = V.toList $ V.map ((toInt <$>)) fs

    return $ MeshData vVecs intFs

mesh :: MeshData -> Material -> Object
mesh mData m =
    let tris = mkTri <$> meshFaces mData
        mkTri is = let v0 = (meshVertices mData) V.! (is V.! 0)
                       v1 = (meshVertices mData) V.! (is V.! 1)
                       v2 = (meshVertices mData) V.! (is V.! 2)
                   in triWithNormals v0 v1 v2 m
    in gridWithMaterial tris m
