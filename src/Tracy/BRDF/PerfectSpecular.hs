module Tracy.BRDF.PerfectSpecular
  ( perfectSpecular
  )
  where

import Control.Lens
import Linear
import Data.Colour

import Tracy.Types

perfectSpecular :: Texture -> Double -> BRDF
perfectSpecular t k =
    BRDF undefined (perfectSpecularSampleF t k) undefined

perfectSpecularSampleF :: Texture -> Double -> Shade -> V3 Double -> TraceM (Double, Color, V3 Double)
perfectSpecularSampleF t k sh wo = do
    let ndotwo = (sh^.normal) `dot` wo
        wi = ((-1) *^ wo) + (2.0 * ndotwo *^ (sh^.normal))

    return ( 1.0 -- not sure if a 1.0 PDF is a sane thing to return, but nothing uses this yet
           , (grey k * ((t^.getColor) sh)) / (grey ((sh^.normal) `dot` wi))
           , wi
           )
