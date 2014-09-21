module Tracy.Objects.Mesh
  ( mesh
  , loadMesh
  ) where

import Control.Applicative
import Data.ByteString.Char8
import qualified Data.Vector as V
import Linear
import PLY
import PLY.Types

import Tracy.Types
import Tracy.Grid
import Tracy.Objects.Triangle

loadMesh :: FilePath -> IO MeshDesc
loadMesh filename = do
    Right vs <- loadElements (pack "vertex") filename
    Right fs <- loadElements (pack "face") filename

    let toFloat (Sfloat f) = f
        toFloat e = error $ "Could not get float from scalar: " ++ show e
        toInt (Suint i) = fromEnum i
        toInt (Sint i) = i
        toInt e = error $ "Could not get int from scalar: " ++ show e

        vVecs = mkVec <$> V.toList vs
        mkVec vals = V3 (toFloat $ vals V.! 0) (toFloat $ vals V.! 1) (toFloat $ vals V.! 2)

        -- Vector (Vector Scalar) -> Vector [Int] -> [[Int]]
        intFs = V.toList $ V.map ((toInt <$>) . V.toList) fs

    return $ MeshDesc vVecs intFs

mesh :: MeshDesc -> Material -> Object
mesh mDesc m =
    let tris = mkTri <$> meshDescFaces mDesc
        mkTri is = let v0 = (meshDescVertices mDesc) !! (is !! 0)
                       v1 = (meshDescVertices mDesc) !! (is !! 1)
                       v2 = (meshDescVertices mDesc) !! (is !! 2)
                   in tri v0 v1 v2 m
    in grid tris
