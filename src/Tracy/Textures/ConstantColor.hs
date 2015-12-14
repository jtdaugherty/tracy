module Tracy.Textures.ConstantColor
  ( constantColor
  )
where

import Tracy.Types

constantColor :: Color -> Texture
constantColor c =
    Texture { _getColor = const c
            }
