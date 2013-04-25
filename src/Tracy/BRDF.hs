module Tracy.BRDF where

import Control.Lens
import Linear
import Data.Colour

import Tracy.Types
import Tracy.Constants

lambertian :: Sampler (V3 Double) -> Color -> Double -> BRDF
lambertian s cd kd =
    let dat = BRDFData kd cd
    in BRDF lambFunc lambSample lambRhoFunc s dat

glossySpecular :: Color -> Double -> BRDF
glossySpecular ks glossyExp =
    BRDF (glossySpecularFunc ks glossyExp) undefined glossyRhoFunc undefined undefined

lambFunc :: BRDFData -> Shade -> V3 Double -> V3 Double -> Color
lambFunc dat _ _ _ = (grey $ dat^.brdfKD) * (dat^.brdfColor) * (grey invPI)

lambSample :: BRDFData -> Shade -> V3 Double -> V3 Double -> V3 Double -> (Double, Color)
lambSample dat sh _ _ sp =
    let w = sh^.normal
	v = signorm $ V3 0.0034 1 0.0071 `cross` w
	u = v `cross` w
	wi = signorm $ (sp^._x *^ u) + (sp^._y *^ v) + (sp^._z *^ w)

	pdf = (sh^.normal) `dot` (wi ^* invPI)

    in (pdf, grey (dat^.brdfKD) * (dat^.brdfColor) * grey invPI)

lambRhoFunc :: BRDFData -> Shade -> V3 Double -> Color
lambRhoFunc dat _ _ = grey (dat^.brdfKD) * (dat^.brdfColor)

glossySpecularFunc :: Color -> Double -> BRDFData
                   -> Shade -> V3 Double -> V3 Double -> Color
glossySpecularFunc ks glossyExp _dat sh wi wo =
    let ndotwi = (sh^.normal) `dot` wi
        r = ((-1) *^ wi) + (2.0 * ndotwi *^ sh^.normal)
        rdotwo = r `dot` wo
    in if rdotwo > 0
       then ks * (grey $ rdotwo ** glossyExp)
       else cBlack

glossyRhoFunc :: BRDFData -> Shade -> V3 Double -> Color
glossyRhoFunc _ _ _ = cBlack
