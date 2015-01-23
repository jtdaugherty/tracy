{-# LANGUAGE ForeignFunctionInterface #-}
module Tracy.Util
  ( max3
  , min3
  , getColorBytes
  , defaultShade
  , clamp
  , dSquared
  , toV3
  , toV4

  , createMergeBuffer
  , mergeBatches
  , vectorFromMergeBuffer
  ) where

import Control.Applicative
import Control.Lens
import Data.Colour
import Linear
import qualified Data.ByteString as B
import qualified Data.Vector.Storable as SV

import Foreign (mallocArray)
import Foreign.Storable
import Foreign.C.Types
import Foreign.Ptr

import Tracy.Types

getColorBytes :: Colour -> B.ByteString
getColorBytes (Colour r g b) =
    B.pack $ (toEnum . fromEnum) <$> [ r * 255, g * 255, b * 255, 255 ]

defaultShade :: Shade
defaultShade =
    Shade { _localHitPoint = V3 0 0 0
          , _normal = V3 0 0 0
          , _material = error "no material set on Shade"
          , _shadeRay = Ray (V3 0 0 0) (V3 0 0 0)
          , _depth = 0
          }

clamp :: (Ord a) => a -> a -> a -> a
clamp v mnB mxB = if v < mnB
                  then mnB else if v > mxB
                                then mxB else v

mx :: (Ord a) => a -> a -> a
mx a b = if a > b then a else b

mn :: (Ord a) => a -> a -> a
mn a b = if a < b then a else b

max3 :: (Ord a) => a -> a -> a -> a
max3 a b c = mx a $ mx b c

min3 :: (Ord a) => a -> a -> a -> a
min3 a b c = mn a $ mn b c

dSquared :: (Floating a, Num a) => V3 a -> V3 a -> a
dSquared v1 v2 = (v1^._x - v2^._x) ** 2 +
                 (v1^._y - v2^._y) ** 2 +
                 (v1^._z - v2^._z) ** 2

createMergeBuffer :: Int -> Int -> IO (Int, Int, Ptr Double)
createMergeBuffer rows cols = do
    p <- mallocArray (3 * rows * cols)
    return (rows, cols, p)

mergeBatches :: Int -> (Int, Int, Ptr Double) -> SV.Vector Color -> IO ()
mergeBatches numSamples (rows, cols, merged) newData =
    SV.unsafeWith newData $ \p ->
        c_running_average
          (fromIntegral numSamples)
          (fromIntegral $ 3 * rows * cols)
          merged
          (castPtr p)

vectorFromMergeBuffer :: (Int, Int, Ptr Double) -> IO (SV.Vector Color)
vectorFromMergeBuffer (rows, cols, merged) =
    SV.generateM (rows*cols) $ peekElemOff (castPtr merged)

foreign import ccall unsafe "running_average"
  c_running_average :: CDouble -> CInt -> Ptr Double -> Ptr Double -> IO ()

toV4 :: (Num a) => V3 a -> V4 a
toV4 v = V4 (v^._x) (v^._y) (v^._z) 0

toV3 :: V4 a -> V3 a
toV3 v = V3 (v^._x) (v^._y) (v^._z)
