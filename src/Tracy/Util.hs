{-# LANGUAGE ForeignFunctionInterface, BangPatterns #-}
module Tracy.Util
  ( max3
  , min3
  , getColorBytes
  , defaultShade
  , clamp
  , dSquared
  , toV3
  , toV4
  , flipNormal
  , maxToOne

  , createMergeBuffer
  , mergeChunks
  , vectorFromMergeBuffer

  , (!*.)
  ) where

import Control.Applicative
import Data.Colour
import Linear
import qualified Data.ByteString as B
import qualified Data.Vector.Storable as SV

import Foreign (mallocArray)
import Foreign.Storable
import Foreign.C.Types
import Foreign.Ptr

import Tracy.Types

maxToOne :: Color -> Color
maxToOne (Colour r g b) = Colour r' g' b'
    where
      m = max3 r g b
      (r', g', b') = if m > 1
                     then (r/m, g/m, b/m)
                     else (r, g, b)

{-# INLINE flipNormal #-}
flipNormal :: V3 Double -> V3 Double -> V3 Double
flipNormal !dir !n = if dir `dot` n > 0 then -1 *^ n else n

{-# INLINE getColorBytes #-}
getColorBytes :: Colour -> B.ByteString
getColorBytes (Colour r g b) =
    B.pack [ round $ r * 255
           , round $ g * 255
           , round $ b * 255
           , 255
           ]

defaultShade :: Shade
defaultShade =
    Shade { _localHitPoint = V3 0 0 0
          , _normal = V3 0 0 0
          , _material = error "no material set on Shade"
          , _shadeRay = Ray (V3 0 0 0) (V3 0 0 0)
          , _depth = Depth 0
          , _mappingU = Nothing
          , _mappingV = Nothing
          }

clamp :: (Ord a) => a -> a -> a -> a
clamp v mnB mxB = if v < mnB
                  then mnB else if v > mxB
                                then mxB else v

{-# INLINE mx #-}
mx :: (Ord a) => a -> a -> a
mx a b = if a > b then a else b

{-# INLINE mn #-}
mn :: (Ord a) => a -> a -> a
mn a b = if a < b then a else b

{-# INLINE max3 #-}
max3 :: (Ord a) => a -> a -> a -> a
max3 a b c = mx a $ mx b c

{-# INLINE min3 #-}
min3 :: (Ord a) => a -> a -> a -> a
min3 a b c = mn a $ mn b c

dSquared :: (Floating a, Num a) => V3 a -> V3 a -> a
dSquared (V3 x1 y1 z1) (V3 x2 y2 z2) =
    (x1 - x2) ** 2 + (y1 - y2) ** 2 + (z1 - z2) ** 2

createMergeBuffer :: Int -> Int -> IO (Int, Int, Ptr Double)
createMergeBuffer rows cols = do
    p <- mallocArray (3 * rows * cols)
    return (rows, cols, p)

mergeChunks :: Int -> Int -> Row -> (Int, Int, Ptr Double) -> SV.Vector Color -> IO ()
mergeChunks numSamples newSampleCount (Row startRow) (_, cols, merged) newData = do
    SV.unsafeWith newData $ \p ->
        c_running_average
          (fromIntegral numSamples)
          (fromIntegral newSampleCount)
          (fromIntegral $ 3 * SV.length newData)
          (plusPtr merged $ startRow * cols * 3 * (sizeOf (0::Double)))
          (castPtr p)

vectorFromMergeBuffer :: (Int, Int, Ptr Double) -> IO (SV.Vector Color)
vectorFromMergeBuffer (rows, cols, merged) =
    SV.generateM (rows*cols) $ peekElemOff (castPtr merged)

foreign import ccall unsafe "running_average"
  c_running_average :: CDouble -> CDouble -> CInt -> Ptr Double -> Ptr Double -> IO ()

{-# INLINE toV4 #-}
toV4 :: (Num a) => V3 a -> V4 a
toV4 (V3 x y z) = V4 x y z 0

{-# INLINE toV3 #-}
toV3 :: V4 a -> V3 a
toV3 (V4 x y z _) = V3 x y z

{-# INLINE (!*.) #-}
(!*.) :: M44 Double -> V3 Double -> V3 Double
m !*. (V3 x y z) = toV3 $ m !* v'
    where
      v' = V4 x y z 1
