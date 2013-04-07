module Tracy.Plane where

import Control.Lens
import Linear

import Tracy.Types
import Tracy.Constants

plane :: V3 Double -> V3 Double -> Color -> Object
plane o n c =
    Object { _objectColor = c
           , _hit = hitPlane o n c
           }

hitPlane :: V3 Double -> V3 Double -> Color -> Ray -> Maybe (Shade, Double)
hitPlane o n c ray =
    let t = ((o - (ray^.origin)) `dot` n) / ((ray^.direction) `dot` n)
        s = Shade { _localHitPoint = ray^.origin + (t *^ ray^.direction)
                  , _normal = n
                  , _shadeColor = c
                  }
    in if t <= epsilon
       then Nothing
       else Just (s, t)