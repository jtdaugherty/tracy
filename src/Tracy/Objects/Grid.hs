{-# LANGUAGE ParallelListComp, BangPatterns #-}
module Tracy.Objects.Grid
  ( grid
  , gridWithMaterial
  )
  where

import Tracy.Types
import Tracy.Constants
import Tracy.BoundingBox
import Tracy.Util
import Tracy.Objects.Compound
import Data.Maybe
import qualified Data.Vector as V
import Control.Lens hiding (ix, inside)
import Control.Applicative
import Control.Monad.State
import qualified Data.Map as M
import Linear

grid :: V.Vector Object -> Object
grid os = grid_ os Nothing

gridWithMaterial :: V.Vector Object -> Material -> Object
gridWithMaterial os m = grid_ os (Just m)

grid_ :: V.Vector Object -> Maybe Material -> Object
grid_ os mMat =
    let bbox = boundingBox (minCoords os) (maxCoords os)
        dims = getDimensions os
        hitF = hitGrid dims bbox $ setupCells bbox os dims
    in Object { _objectMaterial = case mMat of
                                    Just m -> m
                                    Nothing -> error "should not use objectMaterial of grid"
              , _hit = hitF
              , _shadow_hit = (snd <$>) . hitF
              , _bounding_box = Just bbox
              , _areaLightImpl = Nothing
              }

minCoords :: V.Vector Object -> V3 Double
minCoords os =
    V3 (mx - epsilon) (my - epsilon) (mz - epsilon)
    where
      boxes = V.map (^.bounding_box.to fromJust) os
      mx = V.minimum $ V.map (^.bboxP0._x) boxes
      my = V.minimum $ V.map (^.bboxP0._y) boxes
      mz = V.minimum $ V.map (^.bboxP0._z) boxes

maxCoords :: V.Vector Object -> V3 Double
maxCoords os =
    V3 (mx + epsilon) (my + epsilon) (mz + epsilon)
    where
      boxes = V.map (^.bounding_box.to fromJust) os
      mx = V.maximum $ V.map (^.bboxP1._x) boxes
      my = V.maximum $ V.map (^.bboxP1._y) boxes
      mz = V.maximum $ V.map (^.bboxP1._z) boxes

getDimensions :: V.Vector Object -> (Int, Int, Int)
getDimensions os = (truncate nx, truncate ny, truncate nz)
    where
      p0 = minCoords os
      p1 = maxCoords os

      wx = p1^._x - p0^._x
      wy = p1^._y - p0^._y
      wz = p1^._z - p0^._z

      multiplier = 2.0
      s = ((wx * wy * wz) / toEnum (V.length os)) ** 0.3333333

      nx = multiplier * wx / s + 1
      ny = multiplier * wy / s + 1
      nz = multiplier * wz / s + 1

setupCells :: BBox -> V.Vector Object -> (Int, Int, Int) -> M.Map (Int, Int, Int) Object
setupCells b os (nx, ny, nz) = mkCompounds $ V.foldr addObject M.empty os
    where
      p0 = b^.bboxP0
      p1 = b^.bboxP1
      addObject :: Object -> M.Map (Int, Int, Int) (V.Vector Object) -> M.Map (Int, Int, Int) (V.Vector Object)
      addObject o m = let Just !ob = o^.bounding_box

                          !ixmin = clamp (fromEnum $ (ob^.bboxP0._x - p0^._x) * (toEnum nx) / (p1^._x - p0^._x)) 0 (nx - 1)
                          !iymin = clamp (fromEnum $ (ob^.bboxP0._y - p0^._y) * (toEnum ny) / (p1^._y - p0^._y)) 0 (ny - 1)
                          !izmin = clamp (fromEnum $ (ob^.bboxP0._z - p0^._z) * (toEnum nz) / (p1^._z - p0^._z)) 0 (nz - 1)

                          !ixmax = clamp (fromEnum $ (ob^.bboxP1._x - p0^._x) * (toEnum nx) / (p1^._x - p0^._x)) 0 (nx - 1)
                          !iymax = clamp (fromEnum $ (ob^.bboxP1._y - p0^._y) * (toEnum ny) / (p1^._y - p0^._y)) 0 (ny - 1)
                          !izmax = clamp (fromEnum $ (ob^.bboxP1._z - p0^._z) * (toEnum nz) / (p1^._z - p0^._z)) 0 (nz - 1)

                          ins Nothing = Just $ V.singleton o
                          ins (Just !xs) = Just $ V.cons o xs
                          addToCell = M.alter ins
                          !is = [ (x, y, z)
                                | z <- [izmin..izmax]
                                , y <- [iymin..iymax]
                                , x <- [ixmin..ixmax]
                                ]
                      in (foldr (.) id $ addToCell <$> is) m

      mkCompounds m = M.map (\objs -> compound objs undefined) m

data St = St { txNext :: !Double
             , tyNext :: !Double
             , tzNext :: !Double
             , ix :: !Int
             , iy :: !Int
             , iz :: !Int
             }
             deriving Show

hitGrid :: (Int, Int, Int) -> BBox -> M.Map (Int, Int, Int) Object -> Ray -> Maybe (Shade, Double)
hitGrid (nx, ny, nz) !bbox m !ray =
    if t0 > t1
       then Nothing
       else let !st = St tx_next ty_next tz_next iix iiy iiz
            in evalState findHit st
    where
        !ox = ray^.origin._x
        !oy = ray^.origin._y
        !oz = ray^.origin._z
        !dx = ray^.direction._x
        !dy = ray^.direction._y
        !dz = ray^.direction._z
        !x0 = bbox^.bboxP0._x
        !y0 = bbox^.bboxP0._y
        !z0 = bbox^.bboxP0._z
        !x1 = bbox^.bboxP1._x
        !y1 = bbox^.bboxP1._y
        !z1 = bbox^.bboxP1._z

        !a = 1.0 / dx
        (!tx_min, !tx_max) = if a >= 0
                             then ((x0 - ox) * a, (x1 - ox) * a)
                             else ((x1 - ox) * a, (x0 - ox) * a)
        !b = 1.0 / dy
        (!ty_min, !ty_max) = if b >= 0
                             then ((y0 - oy) * b, (y1 - oy) * b)
                             else ((y1 - oy) * b, (y0 - oy) * b)
        !c = 1.0 / dz
        (!tz_min, !tz_max) = if c >= 0
                             then ((z0 - oz) * c, (z1 - oz) * c)
                             else ((z1 - oz) * c, (z0 - oz) * c)

        !t0 = max3 tx_min ty_min tz_min
        !t1 = min3 tx_max ty_max tz_max

        iix, iiy, iiz :: Int
        (!iix, !iiy, !iiz) = if inside bbox (ray^.origin)
                             then ( clamp (fromEnum $ (ox - x0) * (toEnum nx) / (x1 - x0)) 0 (nx - 1)
                                  , clamp (fromEnum $ (oy - y0) * (toEnum ny) / (y1 - y0)) 0 (ny - 1)
                                  , clamp (fromEnum $ (oz - z0) * (toEnum nz) / (z1 - z0)) 0 (nz - 1)
                                  )
                             else let !p = ray^.origin + t0 *^ ray^.direction
                                  in ( clamp (fromEnum $ (p^._x - x0) * (toEnum nx) / (x1 - x0)) 0 (nx - 1)
                                     , clamp (fromEnum $ (p^._y - y0) * (toEnum ny) / (y1 - y0)) 0 (ny - 1)
                                     , clamp (fromEnum $ (p^._z - z0) * (toEnum nz) / (z1 - z0)) 0 (nz - 1)
                                     )

        !dtx = (tx_max - tx_min) / toEnum nx
        !dty = (ty_max - ty_min) / toEnum ny
        !dtz = (tz_max - tz_min) / toEnum nz

        (!tx_next, !ix_step, !ix_stop) = if dx == 0
                                         then ( hugeValue
                                              , -1
                                              , -1
                                              )
                                         else if dx > 0
                                              then ( tx_min + (toEnum iix + 1) * dtx
                                                   , 1
                                                   , nx
                                                   )
                                              else ( tx_min + (toEnum $ nx - iix) * dtx
                                                   , -1
                                                   , -1
                                                   )
        (!ty_next, !iy_step, !iy_stop) = if dy == 0
                                         then ( hugeValue
                                              , -1
                                              , -1
                                              )
                                         else if dy > 0
                                              then ( ty_min + (toEnum iiy + 1) * dty
                                                   , 1
                                                   , ny
                                                   )
                                              else ( ty_min + (toEnum $ ny - iiy) * dty
                                                   , -1
                                                   , -1
                                                   )
        (!tz_next, !iz_step, !iz_stop) = if dz == 0
                                         then ( hugeValue
                                              , -1
                                              , -1
                                              )
                                         else if dz > 0
                                              then ( tz_min + (toEnum iiz + 1) * dtz
                                                   , 1
                                                   , nz
                                                   )
                                              else ( tz_min + (toEnum $ nz - iiz) * dtz
                                                   , -1
                                                   , -1
                                                   )
        findHit = do txn <- gets txNext
                     tyn <- gets tyNext
                     tzn <- gets tzNext

                     ixv <- gets ix
                     iyv <- gets iy
                     izv <- gets iz

                     let o = M.lookup (ixv, iyv, izv) m
                         hitX = do let rest = do
                                         modify $ \s -> s { txNext = txNext s + dtx, ix = ix s + ix_step }
                                         ix' <- gets ix
                                         if ix' == ix_stop then return Nothing
                                                           else findHit
                                   case o of
                                     Nothing -> rest
                                     Just obj -> case (obj^.hit) ray of
                                                   Nothing -> rest
                                                   Just (sh, t) -> if t < txn then return (Just (sh, t)) else rest
                         hitY = do let rest = do
                                         modify $ \s -> s { tyNext = tyNext s + dty, iy = iy s + iy_step }
                                         iy' <- gets iy
                                         if iy' == iy_stop then return Nothing
                                                           else findHit
                                   case o of
                                     Nothing -> rest
                                     Just obj -> case (obj^.hit) ray of
                                                   Nothing -> rest
                                                   Just (sh, t) -> if t < tyn then return (Just (sh, t)) else rest
                         hitZ = do let rest = do
                                         modify $ \s -> s { tzNext = tzNext s + dtz, iz = iz s + iz_step }
                                         iz' <- gets iz
                                         if iz' == iz_stop then return Nothing
                                                           else findHit
                                   case o of
                                     Nothing -> rest
                                     Just obj -> case (obj^.hit) ray of
                                                   Nothing -> rest
                                                   Just (sh, t) -> if t < tzn then return (Just (sh, t)) else rest

                     if txn < tyn && txn < tzn then hitX else if tyn < tzn then hitY else hitZ
