
module Hanan where

import Geometry
import Shapes
import Data.List as L
import qualified Data.Map as M
import Debug.Trace

{-
segments3d _ (LayerN l
  , Shape (R (P (x, y)) (P (x', y')))) =
    [ (P3 x y l, P3 x y' l)
    , (P3 x y l, P3 x' y l)
    , (P3 x y' l, P3 x' y' l)
    , (P3 x' y l, P3 x' y' l) ]

segments3d _ (_, Obstacle _) = []

segments3d p (LayerN l, Via (P (x, y))) =
  [ (P3 x y (l * spacing p), P3 x y ((l+1) * spacing p)) ]
-}
points3dR3 (R3 (P3 x1 y1 z1) (P3 x2 y2 z2)) =
  [ P3 x y z |
    x <- L.nub [x1, x2]
  , y <- L.nub [y1, y2]
  , z <- L.nub [z1, z2] ]

segments (R3 (P3 x1 y1 z1) (P3 x2 y2 z2))
  | z1 == z2 = rmdups $ [ (P3 x1 y1 z1, P3 x2 y1 z1)
                        , (P3 x1 y1 z1, P3 x1 y2 z1)
                        , (P3 x2 y2 z1, P3 x1 y2 z1)
                        , (P3 x2 y2 z1, P3 x2 y1 z1) ]
  | otherwise = [ (P3 x1 y1 z1, P3 x2 y2 z2) ]


points3dShape p = rmdups . points3dR3 . make3D p

hananSegs :: [Point3D] -> [(Point3D, Point3D)]
hananSegs ps = xsegs ++ ysegs ++ zsegs
 where xs = rmdups (map x ps)
       ys = rmdups (map y ps)
       zs = rmdups (map z ps)
       segs es = zip es (tail es)
       xsegs =
         [(P3 px py pz, P3 px' py pz) |
           (px, px') <- segs xs
         , py <- ys
         , pz <- zs ]
       ysegs =
         [(P3 px py pz, P3 px py' pz) |
           (py, py') <- segs ys
         , px <- xs
         , pz <- zs ]
       zsegs =
         [(P3 px py pz, P3 px py pz') |
           (pz, pz') <- segs zs
         , px <- xs
         , py <- ys ]


hananMap :: [(Point3D, Point3D)] -> M.Map Point3D [(Point3D, Point3D)]
hananMap ps = foldr ih M.empty ps
  where
    ih (seg@(p1, p2)) m =
          M.insertWith (++) p2 [seg] $
            M.insertWith (++) p1 [seg] m


hananPs :: [Point3D] -> [Point3D]
hananPs ps =
  [ P3 px py pz
    | px <- xs
  , py <- ys
  , pz <- zs ]
 where xs = rmdups (map x ps)
       ys = rmdups (map y ps)
       zs = rmdups (map z ps)


points3d :: Problem -> [Point3D]
points3d p = ps -- map (setViaCost (viaCost p)) ps
  where
    ps = concatMap (points3dShape p) (pelements p ++ pvias p)
    setViaCost vc (P3 x y z) = (P3 x y (z * vc))


rmdups :: Ord a => [a] -> [a]
rmdups = L.map L.head . L.group . L.sort

hanan =
  hananPs . points3d



type Segment = (Point3D, Point3D)

segmentToShape :: (Point3D, Point3D) -> (LayerN, Shape)

segmentToShape seg@(P3 x y z, P3 x' y' z')
  | z == z' && x == x' = (LayerN z, Vline $ R (P (x, y)) (P (x', y')))
  | z == z' && y == y' = (LayerN z, Hline $ R (P (x, y)) (P (x', y')))
  | z /= z' && x == x' && y == y' = (LayerN (min z z'), AddedVia $ P (x, y))
  | otherwise = error $ "This segment is not allowed: " ++ show seg


makeSolution :: [(Point3D, Point3D)] -> Problem -> Solution
makeSolution pts p =
  Solution { selements = filter (not . isVia) segs
           , svias = filter isVia segs
           , sMetalLayers = metalLayers p }
  where
    vc = viaCost p
    fixLayer (LayerN n, x) = (LayerN $ n `div` vc, x)
    segs = map (fixLayer . segmentToShape) pts
    isVia (_, AddedVia _) = True
    isVia (_, _) = False

-- This is to eliminate obstacles
--hananSolution p = makeSolution (filterObstacles p . hananSegs . points3d $ p) p
hananSolution p = makeSolution (segs ++ vias) p
  where segs = filterAlwaysIn p . hananSegs . points3d $ p
        vias = concatMap segments $ getRoutedVias3D p

  {-undefined
inSolution p segs = segs
  where
  any (collides r)
  -}


isVia (p1, p2) = z p1 /= z p2


getRoutedVias3D p = map (make3D p) $ pvias p

filterAlwaysIn p segs = filter bothEndsInShape $ filter (not . isVia) segs
  where ss = map (make3D p) $ getShapes p
        bothEndsInShape (p1, p2) = isInAnyShape p1 && isInAnyShape p2
        isInAnyShape pt = any (flip collidesP pt) ss


filterObstacles p segs = filter collidesAnySeg segs
  where obs = map ((flip resize) (-1) . make3D p) $ getObstacles p
        collidesAny pt = any (flip collidesP pt) obs
        collidesAnySeg (p1, p2) = not $ collidesAny p1 || collidesAny p2


prepareProblem p = undefined
  where
    alwaysIn = map (make3D p) $ getShapes p ++ pvias p
    obs = map (make3D p) $ getObstacles p





