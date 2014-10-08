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
    Material { _doShading = emissiveAreaShading c ls
             , _doAreaShading = emissiveAreaShading c ls
             , _getLe = const $ grey (float2Double ls) * c
             }

emissiveAreaShading :: Color -> Float -> Shade -> Tracer -> TraceM Color
emissiveAreaShading c ls sh _ = do
    if ((-1) *^ (sh^.normal)) `dot` (sh^.shadeRay.direction) > 0 then
       return (grey (float2Double ls) * c) else
       return cBlack
