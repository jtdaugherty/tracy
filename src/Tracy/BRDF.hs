module Tracy.BRDF where

import Control.Lens
import Linear
import Data.Colour
import GHC.Float

import Tracy.Types
import Tracy.Constants

lambertian :: Sampler (V3 Float) -> Color -> Float -> BRDF
lambertian s cd kd =
    let dat = BRDFData kd cd
    in BRDF lambFunc lambSample lambRhoFunc s dat

glossySpecular :: Color -> Float -> BRDF
glossySpecular ks glossyExp =
    BRDF (glossySpecularFunc ks glossyExp) undefined glossyRhoFunc undefined undefined

lambFunc :: BRDFData -> Shade -> V3 Float -> V3 Float -> Color
lambFunc dat _ _ _ = (grey $ float2Double $ dat^.brdfKD) * (dat^.brdfColor) * (grey $ float2Double invPI)

lambSample :: BRDFData -> Shade -> V3 Float -> V3 Float -> V3 Float -> (Float, Color)
lambSample dat sh _ _ sp =
    let w = sh^.normal
	v = signorm $ V3 0.0034 1 0.0071 `cross` w
	u = v `cross` w
	wi = signorm $ (sp^._x *^ u) + (sp^._y *^ v) + (sp^._z *^ w)

	pdf = (sh^.normal) `dot` (wi ^* invPI)

    in (pdf, grey (float2Double $ dat^.brdfKD) * (dat^.brdfColor) * (grey $ float2Double invPI))

lambRhoFunc :: BRDFData -> Shade -> V3 Float -> Color
lambRhoFunc dat _ _ = grey (float2Double $ dat^.brdfKD) * (dat^.brdfColor)

glossySpecularFunc :: Color -> Float -> BRDFData
                   -> Shade -> V3 Float -> V3 Float -> Color
glossySpecularFunc ks glossyExp _dat sh wi wo =
    let ndotwi = (sh^.normal) `dot` wi
        r = ((-1) *^ wi) + (2.0 * ndotwi *^ sh^.normal)
        rdotwo = r `dot` wo
    in if rdotwo > 0
       then ks * (grey $ float2Double $ rdotwo ** glossyExp)
       else cBlack

glossyRhoFunc :: BRDFData -> Shade -> V3 Float -> Color
glossyRhoFunc _ _ _ = cBlack
