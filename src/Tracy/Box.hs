module Tracy.Box where

import Linear

import Tracy.Types
import Tracy.BoundingBox
import Tracy.Util

box :: V3 Float -> V3 Float -> Material -> Object
box p0 p1 m =
    Object { _objectMaterial = m
           , _hit = hitBox p0 p1 m
           , _shadow_hit = shadowHitBox p0 p1
           , _bounding_box = Just $ boundingBox p0 p1
           }

shadowHitBox :: V3 Float -> V3 Float -> Ray -> Maybe Float
shadowHitBox p0 p1 r =
   case boundingBoxHit (boundingBox p0 p1) r of
       Nothing -> Nothing
       Just (_, _, t, _) -> Just t

hitBox :: V3 Float -> V3 Float -> Material -> Ray -> Maybe (Shade, Float)
hitBox p0 p1 m r =
    let bbox = boundingBox p0 p1
    in case boundingBoxHit bbox r of
        Nothing -> Nothing
        Just (_, faceNorm, t, localHP) ->
            let sh = defaultShade { _localHitPoint = localHP
                                  , _material = m
                                  , _normal = faceNorm
                                  }
            in Just (sh, t)
