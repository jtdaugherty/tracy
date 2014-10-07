module Tracy.Materials.Emissive
  ( emissive
  )
  where

import Control.Lens
import Data.Colour
import GHC.Float
import Linear

import Tracy.Types

emissive :: Color -> Float -> Material
emissive c ls =
    Material { _doShading = emissiveShading c ls
             , _doAreaShading = emissiveAreaShading c ls
             }

emissiveShading :: Color -> Float -> Shade -> TraceM Color
emissiveShading _ _ _ = return cBlack

emissiveAreaShading :: Color -> Float -> Shade -> TraceM Color
emissiveAreaShading c ls sh = do
    if ((-1) *^ (sh^.normal)) `dot` (sh^.shadeRay.direction) > 0 then
       return (grey (float2Double ls) * c) else
       return cBlack
