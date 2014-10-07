module Tracy.Tracers
  ( rayCastTracer
  )
  where

import Control.Applicative
import Control.Lens
import Data.List
import Data.Maybe
import Data.Ord (comparing)

import Tracy.Types

rayCastTracer :: Tracer
rayCastTracer =
    Tracer { _doTrace = rayCastTrace
           }

rayCastTrace :: Ray -> TraceM Color
rayCastTrace ray = do
    v <- doHit ray
    case v of
        Nothing -> view $ tdWorld.bgColor
        Just (sh, _t) -> (sh^.material.doShading) (sh & shadeRay .~ ray)

doHit :: Ray -> TraceM (Maybe (Shade, Float))
doHit r = do
    hitFuncs <- view tdWorldHitFuncs
    return $ listToMaybe $ sortBy (comparing snd) $ catMaybes $ hitFuncs <*> pure r
