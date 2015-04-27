module Tracy.Tracers
  ( rayCastTracer
  , areaLightTracer
  , pathTracer
  )
  where

import Control.Lens
import Data.Maybe
import Data.Colour
import Data.Ord (comparing)
import qualified Data.Vector as V

import Tracy.Types

rayCastTracer :: Tracer
rayCastTracer =
    Tracer { _doTrace = rayCastTrace
           }

rayCastTrace :: Ray -> Depth -> TraceM Color
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

pathTrace :: Ray -> Depth -> TraceM Color
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
    let results = V.map fromJust $ V.filter isJust $ V.map ($ r) hitFuncs
    return $ if V.null results
             then Nothing
             else Just (V.minimumBy (comparing snd) results)

areaLightTracer :: Tracer
areaLightTracer =
    Tracer { _doTrace = areaLightTrace
           }

areaLightTrace :: Ray -> Depth -> TraceM Color
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
