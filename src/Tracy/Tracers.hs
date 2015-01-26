module Tracy.Tracers
  ( rayCastTracer
  , areaLightTracer
  , whittedTracer
  , pathTracer
  )
  where

import Control.Applicative
import Control.Lens
import Data.List
import Data.Maybe
import Data.Colour
import Data.Ord (comparing)

import Tracy.Types

rayCastTracer :: Tracer
rayCastTracer =
    Tracer { _doTrace = rayCastTrace
           }

rayCastTrace :: Ray -> Int -> TraceM Color
rayCastTrace ray theDepth = do
    maxD <- view (tdWorld.viewPlane.maxDepth)
    if theDepth > maxD then
        return cBlack else
        do
          v <- doHit ray
          case v of
              Nothing -> view $ tdWorld.bgColor
              Just (sh, _t) -> (sh^.material.doShading)
                                 (sh & shadeRay .~ ray & depth .~ theDepth)
                                 rayCastTracer

pathTracer :: Tracer
pathTracer =
    Tracer { _doTrace = pathTrace
           }

pathTrace :: Ray -> Int -> TraceM Color
pathTrace ray theDepth = do
    maxD <- view (tdWorld.viewPlane.maxDepth)
    if theDepth > maxD then
        return cBlack else
        do
          v <- doHit ray
          case v of
              Nothing -> view $ tdWorld.bgColor
              Just (sh, _t) -> (sh^.material.doPathShading)
                                 (sh & shadeRay .~ ray & depth .~ theDepth)
                                 pathTracer

doHit :: Ray -> TraceM (Maybe (Shade, Double))
doHit r = do
    hitFuncs <- view tdWorldHitFuncs
    return $ listToMaybe $ sortBy (comparing snd) $ catMaybes $ hitFuncs <*> pure r

areaLightTracer :: Tracer
areaLightTracer =
    Tracer { _doTrace = areaLightTrace
           }

areaLightTrace :: Ray -> Int -> TraceM Color
areaLightTrace ray theDepth = do
    maxD <- view (tdWorld.viewPlane.maxDepth)
    if theDepth > maxD then
        return cBlack else
        do
          v <- doHit ray
          case v of
              Nothing -> view $ tdWorld.bgColor
              Just (sh, _t) -> (sh^.material.doAreaShading)
                                 (sh & shadeRay .~ ray & depth .~ theDepth)
                                 areaLightTracer

whittedTracer :: Tracer
whittedTracer =
    Tracer { _doTrace = whittedTrace
           }

whittedTrace :: Ray -> Int -> TraceM Color
whittedTrace ray theDepth = do
    maxD <- view (tdWorld.viewPlane.maxDepth)
    if theDepth > maxD then
        return cBlack else
        do
          v <- doHit ray
          case v of
              Nothing -> view $ tdWorld.bgColor
              Just (sh, _t) -> (sh^.material.doShading)
                                 (sh & shadeRay .~ ray & depth .~ theDepth)
                                 whittedTracer
