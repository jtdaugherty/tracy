module Tracy.Materials.Add
  ( add
  )
  where

import Control.Lens
import Control.Monad

import Tracy.Types

add :: Material -> Material -> Material
add m1 m2 =
    Material { _doShading = addShading m1 m2
             , _doAreaShading = addAreaShading m1 m2
             , _doPathShading = addPathShading m1 m2
             , _getLe = addGetLe m1 m2
             }

(+++) :: (Monad m, Num a) => m a -> m a -> m a
a +++ b = liftM2 (+) a b

addShading :: Material -> Material -> Shade -> Tracer -> TraceM Color
addShading m1 m2 sh t = v1 +++ v2
  where
    v1 = (m1^.doShading) sh t
    v2 = (m2^.doShading) sh t

addAreaShading :: Material -> Material -> Shade -> Tracer -> TraceM Color
addAreaShading m1 m2 sh t = v1 +++ v2
  where
    v1 = (m1^.doAreaShading) sh t
    v2 = (m2^.doAreaShading) sh t

addPathShading :: Material -> Material -> Shade -> Tracer -> TraceM Color
addPathShading m1 m2 sh t = v1 +++ v2
  where
    v1 = (m1^.doPathShading) sh t
    v2 = (m2^.doPathShading) sh t

addGetLe :: Material -> Material -> Shade -> Color
addGetLe m1 m2 sh = v1 + v2
    where
      v1 = (m1^.getLe) sh
      v2 = (m2^.getLe) sh
