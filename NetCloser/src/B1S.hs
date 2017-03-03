module B1S where

import Hanan
import Kruskal
import Geometry
import Shapes
import Graph


import Control.Monad.State
import qualified Data.Set as S


data Result = Result { score :: Int
                     , tocheck :: [Segment]
                     , using :: [Segment] }

type BS1State a = State Result a



runBS1 :: Problem -> [Segment]
runBS1 p =
  flip evalState (Result 0 todo vias) $ do
    return $ r
  where
    -- | BUG: not getting all the points in the shape
    routed = hannanPointsShapes p
    --routed = map (points3dR3 . make3D p) (getShapes p)
    vias = concatMap segments $ getRoutedVias3D p
    startingSet = segs ++ vias
    candidates = filterObstacles p . hananSegs . points3d $ p
    segs = filterAlwaysIn p $ candidates
    r = runKruskalMST routed (segs ++ vias ++ candidates)
    todo = S.toList $
      S.fromList candidates `S.difference` S.fromList startingSet

 {-

runBS2 :: Problem -> [Segment]
runBS2 p = r
  --flip evalState (Result 0 todo vias) $ do
    --return $ r
  where
    -- | BUG: not getting all the points in the shape
    routed = hannanPointsShapes p
    --routed = map (points3dR3 . make3D p) (getShapes p)
    vias = concatMap segments $ getRoutedVias3D p
    startingSet = segs ++ vias
    candidates = filterObstacles p . hananSegs . points3d $ p
    segs = filterAlwaysIn p $ candidates
    r = runKruskalMST routed (segs ++ vias ++ candidates)

    todo = S.toList $
      S.fromList candidates `S.difference` S.fromList startingSet

  -}

runBS1' :: Problem -> Solution
runBS1' p = makeSolution (runBS1 p) p
