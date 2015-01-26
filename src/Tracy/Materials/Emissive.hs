module Tracy.Materials.Emissive
  ( emissive
  )
  where

import Control.Lens
import Data.Colour
import Linear

import Tracy.Types

emissive :: Color -> Double -> Material
emissive c ls =
    Material { _doShading = emissiveAreaShading c ls
             , _doAreaShading = emissiveAreaShading c ls
             , _doPathShading = emissiveAreaShading c ls
             , _getLe = const $ grey ls * c
             }

emissiveAreaShading :: Color -> Double -> Shade -> Tracer -> TraceM Color
emissiveAreaShading c ls sh _ = do
    if ((-1) *^ (sh^.normal)) `dot` (sh^.shadeRay.direction) > 0 then
       return (grey ls * c) else
       return cBlack
