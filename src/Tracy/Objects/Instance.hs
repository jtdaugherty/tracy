module Tracy.Objects.Instance
  ( inst
  )
  where

import Control.Applicative
import Control.Lens
import Data.Distributive
import Linear

import Tracy.Types
import Tracy.Util

inst :: Transformation -> Maybe Material -> Object -> Object
inst trans newMat o =
    let Trans (tForward, tInverse) = trans
        Just theMaterial = newMat <|> Just (o^.objectMaterial)
        bbox = transBBox tForward <$> o^.bounding_box
    in Object { _objectMaterial = theMaterial
              , _hit = instHit tInverse theMaterial o
              , _shadow_hit = instShadowHit tInverse theMaterial o
              , _bounding_box = bbox
              , _areaLightImpl = transLightImpl trans <$> (o^.areaLightImpl)
              }

transLightImpl :: Transformation -> ObjectAreaLightImpl -> ObjectAreaLightImpl
transLightImpl (Trans (tForward, tInverse)) ali =
    ObjectALI { _objectSurfaceSample = do
                  s <- ali^.objectSurfaceSample
                  return $ tForward !*. s
              , _objectGetNormal = \sh ->
                  -- XXX need to verify that this is supposed to be the
                  -- transpose of the inverse rather than the forward
                  -- transformation
                  signorm $ toV3 $ (distribute tInverse) !*
                    (toV4 $ (ali^.objectGetNormal) sh)
              , _objectPDF = ali^.objectPDF
              }

transBBox :: M44 Float -> BBox -> BBox
transBBox tForward box =
    let p0 = box^.bboxP0
        p1 = box^.bboxP1

        v0 = p0
        v1 = V3 (p1^._x) (p0^._y) (p0^._z)
        v2 = V3 (p1^._x) (p1^._y) (p0^._z)
        v3 = V3 (p0^._x) (p1^._y) (p0^._z)
        v4 = V3 (p0^._x) (p0^._y) (p1^._z)
        v5 = V3 (p1^._x) (p0^._y) (p1^._z)
        v6 = p1
        v7 = V3 (p0^._x) (p1^._y) (p1^._z)

        vs = (tForward !*.) <$> [ v0, v1, v2, v3, v4, v5, v6, v7 ]

        minX = minimum $ vs^..folded._x
        minY = minimum $ vs^..folded._y
        minZ = minimum $ vs^..folded._z

        maxX = maximum $ vs^..folded._x
        maxY = maximum $ vs^..folded._y
        maxZ = maximum $ vs^..folded._z

    in BBox (V3 minX minY minZ) (V3 maxX maxY maxZ)

(!*.) :: M44 Float -> V3 Float -> V3 Float
m !*. v = toV3 $ m !* v'
    where
      v' = V4 (v^._x) (v^._y) (v^._z) 1

instShadowHit :: M44 Float -> Material -> Object -> Ray -> Maybe Float
instShadowHit matrix mat o r = snd <$> instHit matrix mat o r

instHit :: M44 Float -> Material -> Object -> Ray -> Maybe (Shade, Float)
instHit matrix mat o r =
    let inv_ray = Ray { _origin = matrix !*. (r^.origin)
                      , _direction = toV3 $ matrix !* (toV4 $ r^.direction)
                      }

    in case (o^.hit) inv_ray of
        Nothing -> Nothing
        Just (sh, tval) ->
            Just ( sh & normal .~ (signorm $ toV3 $ (distribute matrix) !* (toV4 $ sh^.normal))
                      & material .~ mat
                      & localHitPoint .~ r^.origin + tval *^ r^.direction
                 , tval
                 )
