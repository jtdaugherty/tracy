module Tracy.Objects.Box
  ( box
  )
  where

import Linear

import Tracy.Types
import Tracy.BoundingBox
import Tracy.Util

box :: V3 Double -> V3 Double -> Material -> Object
box p0 p1 m =
    let bbox = boundingBox p0 p1
    in Object { _objectMaterial = m
              , _hit = hitBox bbox m
              , _shadow_hit = shadowHitBox bbox
              , _bounding_box = Just bbox
              , _areaLightImpl = Nothing
              }

shadowHitBox :: BBox -> Ray -> Maybe Double
shadowHitBox bb r =
   case boundingBoxHit bb r of
       Nothing -> Nothing
       Just (_, _, t, _) -> Just t

hitBox :: BBox -> Material -> Ray -> Maybe (Shade, Double)
hitBox bbox m r =
    case boundingBoxHit bbox r of
        Nothing -> Nothing
        Just (_, faceNorm, t, localHP) ->
            let sh = defaultShade { _localHitPoint = localHP
                                  , _material = m
                                  , _normal = faceNorm
                                  }
            in Just (sh, t)
