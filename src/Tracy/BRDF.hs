module Tracy.BRDF where

import Control.Lens
import Linear
import Data.Colour
import GHC.Float

import Tracy.Types
import Tracy.Constants

lambertian :: Sampler (V3 Float) -> Color -> Float -> BRDF
lambertian s cd kd =
    BRDF (lambFunc cd kd) (lambSample cd kd) (lambRhoFunc cd kd) s

glossySpecular :: Color -> Float -> BRDF
glossySpecular ks glossyExp =
    BRDF (glossySpecularFunc ks glossyExp) (glossySpecularSampleF ks glossyExp) glossyRhoFunc undefined

perfectSpecular :: Color -> Float -> BRDF
perfectSpecular c k =
    BRDF undefined (perfectSpecularSampleF c k) undefined undefined

glossySpecularSampleF :: Color -> Float -> Shade -> V3 Float -> TraceM (Float, Color, V3 Float)
glossySpecularSampleF c k sh wo = do
    let ndotwo = (sh^.normal) `dot` wo
        r = ((-1) *^ wo) + (2.0 * ndotwo *^ (sh^.normal))
        w = r
        u = signorm $ V3 0.00424 1 0.00764 `cross` w
        v = u `cross` w

    sp <- view tdHemiSample
    let wi1 = (sp^._x *^ u) + (sp^._y *^ v) + (sp^._z *^ w)
        wi2 = if (sh^.normal) `dot` wi1 < 0
              then ((-1) * (sp^._x) *^ u) + (sp^._y *^ v) + (sp^._z *^ w)
              else wi1
        phong_lobe = (r `dot` wi2) ** k
        pdf = phong_lobe * ((sh^.normal) `dot` wi2)

    return ( pdf
           , (grey $ (float2Double $ k * phong_lobe)) * c
           , wi2
           )

perfectSpecularSampleF :: Color -> Float -> Shade -> V3 Float -> TraceM (Float, Color, V3 Float)
perfectSpecularSampleF c k sh wo = do
    let ndotwo = (sh^.normal) `dot` wo
        wi = ((-1) *^ wo) + (2.0 * ndotwo *^ (sh^.normal))

    return ( 1.0 -- not sure if a 1.0 PDF is a sane thing to return, but nothing uses this yet
           , (grey (float2Double k) * c) / (grey (float2Double $ (sh^.normal) `dot` wi))
           , wi
           )

lambFunc :: Color -> Float -> Shade -> V3 Float -> V3 Float -> Color
lambFunc cd kd _ _ _ = (grey $ float2Double kd) * cd * (grey $ float2Double invPI)

lambSample :: Color -> Float -> Shade -> V3 Float -> TraceM (Float, Color, V3 Float)
lambSample cd kd sh _ = do
    sp <- view tdHemiSample

    let w = sh^.normal
        v = signorm $ V3 0.0034 1 0.0071 `cross` w
        u = v `cross` w
        wi = signorm $ (sp^._x *^ u) + (sp^._y *^ v) + (sp^._z *^ w)
        pdf = (sh^.normal) `dot` (wi ^* invPI)

    return ( pdf
           , grey (float2Double kd) * cd * (grey $ float2Double invPI)
           , wi
           )

lambRhoFunc :: Color -> Float -> Shade -> V3 Float -> Color
lambRhoFunc cd kd _ _ = grey (float2Double kd) * cd

glossySpecularFunc :: Color -> Float -> Shade -> V3 Float -> V3 Float -> Color
glossySpecularFunc ks glossyExp sh wi wo =
    let ndotwi = (sh^.normal) `dot` wi
        r = ((-1) *^ wi) + (2.0 * ndotwi *^ sh^.normal)
        rdotwo = r `dot` wo
    in if rdotwo > 0
       then ks * (grey $ float2Double $ rdotwo ** glossyExp)
       else cBlack

glossyRhoFunc :: Shade -> V3 Float -> Color
glossyRhoFunc _ _ = cBlack
