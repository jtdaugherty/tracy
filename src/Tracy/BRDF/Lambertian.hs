module Tracy.BRDF.Lambertian
  ( lambertian
  )
  where

import Control.Lens
import Linear
import Data.Colour

import Tracy.Types
import Tracy.Constants

lambertian :: Color -> Double -> BRDF
lambertian cd kd =
    BRDF (lambFunc cd kd) (lambSample cd kd) (lambRhoFunc cd kd)

lambFunc :: Color -> Double -> Shade -> V3 Double -> V3 Double -> Color
lambFunc cd kd _ _ _ = (grey kd) * cd * (grey invPI)

lambSample :: Color -> Double -> Shade -> V3 Double -> TraceM (Double, Color, V3 Double)
lambSample cd kd sh _ = do
    sp <- view tdHemiSample

    let w = sh^.normal
        v = signorm $ V3 0.0034 1 0.0071 `cross` w
        u = v `cross` w
        wi = signorm $ (sp^._x *^ u) + (sp^._y *^ v) + (sp^._z *^ w)
        pdf = (sh^.normal) `dot` (wi ^* invPI)

    return ( pdf
           , grey kd * cd * (grey invPI)
           , wi
           )

lambRhoFunc :: Color -> Double -> Shade -> V3 Double -> Color
lambRhoFunc cd kd _ _ = grey kd * cd
