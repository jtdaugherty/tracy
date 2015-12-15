module Tracy.Textures.TransformedTexture
  ( transformedTexture
  )
where

import Control.Lens
import Linear

import Tracy.Types
import Tracy.Util

transformedTexture :: Transformation -> Texture -> Texture
transformedTexture trans t =
    Texture { _getColor = transformedGetColor trans t
            }

transformedGetColor :: Transformation -> Texture -> Shade -> Color
transformedGetColor trans t sh =
    let Trans (_, tInverse) = trans
    in (t^.getColor) (sh & localHitPoint %~ (toV3 . (tInverse !*) . toV4))
