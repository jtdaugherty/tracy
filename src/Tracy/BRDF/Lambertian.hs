module Tracy.BRDF.Lambertian
  ( lambertian
  )
  where

import Control.Lens
import Linear
import Data.Colour

import Tracy.Types
import Tracy.Constants

lambertian :: Texture -> Double -> BRDF
lambertian t kd =
    BRDF (lambFunc t kd) (lambSample t kd) (lambRhoFunc t kd)

lambFunc :: Texture -> Double -> Shade -> V3 Double -> V3 Double -> Color
lambFunc t kd sh _ _ = (grey kd) * ((t^.getColor) sh) * (grey invPI)

lambSample :: Texture -> Double -> Shade -> V3 Double -> TraceM (Double, Color, V3 Double)
lambSample t kd sh _ = do
    sp <- view tdHemiSample

    let w = sh^.normal
        v = signorm $ V3 0.0034 1 0.0071 `cross` w
        u = v `cross` w
        wi = signorm $ (sp^._x *^ u) + (sp^._y *^ v) + (sp^._z *^ w)
        pdf = (sh^.normal) `dot` (wi ^* invPI)

    return ( pdf
           , grey kd * ((t^.getColor) sh) * (grey invPI)
           , wi
           )

lambRhoFunc :: Texture -> Double -> Shade -> V3 Double -> Color
lambRhoFunc t kd sh _ = grey kd * ((t^.getColor) sh)
