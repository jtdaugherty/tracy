module Tracy.BRDF where

import Control.Lens
import Linear
import Data.Colour

import Tracy.Types
import Tracy.Constants

lambertian :: Color -> Double -> BRDF
lambertian cd kd =
    BRDF (lambFunc cd kd) (lambSample cd kd) (lambRhoFunc cd kd)

glossySpecular :: Color -> Double -> Double -> BRDF
glossySpecular c ks e =
    BRDF (glossySpecularFunc c ks e) (glossySpecularSampleF c ks e) glossyRhoFunc

perfectSpecular :: Color -> Double -> BRDF
perfectSpecular c k =
    BRDF undefined (perfectSpecularSampleF c k) undefined

glossySpecularSampleF :: Color -> Double -> Double -> Shade -> V3 Double -> TraceM (Double, Color, V3 Double)
glossySpecularSampleF c ks e sh wo = do
    let ndotwo = (sh^.normal) `dot` wo
        r = ((-1) *^ wo) + (2.0 * ndotwo *^ (sh^.normal))
        w = r
        u = signorm $ V3 0.00424 1 0.00764 `cross` w
        v = u `cross` w

    spFunc <- view tdHemiSampleExp

    let sp = spFunc e
        wi1 = (sp^._x *^ u) + (sp^._y *^ v) + (sp^._z *^ w)
        wi2 = if (sh^.normal) `dot` wi1 < 0
              then ((-1) * (sp^._x) *^ u) - (sp^._y *^ v) + (sp^._z *^ w)
              else wi1
        phong_lobe = (r `dot` wi2) ** ks
        pdf = phong_lobe * ((sh^.normal) `dot` wi2)

    return ( pdf
           , (grey $ (ks * phong_lobe)) * c
           , wi2
           )

perfectSpecularSampleF :: Color -> Double -> Shade -> V3 Double -> TraceM (Double, Color, V3 Double)
perfectSpecularSampleF c k sh wo = do
    let ndotwo = (sh^.normal) `dot` wo
        wi = ((-1) *^ wo) + (2.0 * ndotwo *^ (sh^.normal))

    return ( 1.0 -- not sure if a 1.0 PDF is a sane thing to return, but nothing uses this yet
           , (grey k * c) / (grey ((sh^.normal) `dot` wi))
           , wi
           )

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

glossySpecularFunc :: Color -> Double -> Double -> Shade -> V3 Double -> V3 Double -> Color
glossySpecularFunc c ks e sh wi wo =
    let ndotwi = (sh^.normal) `dot` wi
        r = ((-1) *^ wi) + (2.0 * ndotwi *^ sh^.normal)
        rdotwo = r `dot` wo
    in if rdotwo > 0
       then c * (grey $ ks * (rdotwo ** e))
       else cBlack

glossyRhoFunc :: Shade -> V3 Double -> Color
glossyRhoFunc _ _ = cBlack
