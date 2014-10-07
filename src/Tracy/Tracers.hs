module Tracy.Tracers
  ( rayCastTracer
  )
  where

import Control.Applicative
import Control.Lens
import Control.Monad.Reader
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
    w <- asks tdWorld
    hSample <- asks tdHemiSample
    case v of
        Nothing -> return $ w^.bgColor
        Just (sh, _t) ->
            return $ (sh^.material.doShading) hSample (w^.worldShadows)
                       w (sh & shadeRay .~ ray)

doHit :: Ray -> TraceM (Maybe (Shade, Float))
doHit r = do
    hitFuncs <- asks tdWorldHitFuncs
    return $ listToMaybe $ sortBy (comparing snd) $ catMaybes $ hitFuncs <*> pure r
