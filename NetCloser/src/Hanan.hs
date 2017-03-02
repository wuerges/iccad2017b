
module Hanan where

import Geometry
import Shapes
import Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace

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


points3dShape p = rmdup . points3dR3 . make3D p

hananSegs :: [Point3D] -> [(Point3D, Point3D)]
hananSegs ps = xsegs ++ ysegs ++ zsegs
 where xs = rmdup (map x ps)
       ys = rmdup (map y ps)
       zs = rmdup (map z ps)
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
hananMap ps = foldr ih M.empty $ rmdups ps
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
 where xs = rmdup (map x ps)
       ys = rmdup (map y ps)
       zs = rmdup (map z ps)


points3d :: Problem -> [Point3D]
points3d p =
    concatMap (points3dShape p) (pelements p ++ pvias p)


canonize (p1, p2) = (p1', p2')
  where [p1', p2'] = L.sort [p1, p2]

rmdup :: Ord a => [a] -> [a]
rmdup = L.map L.head . L.group . L.sort

rmdups :: [Segment] -> [Segment]
rmdups = L.map L.head . L.group . L.sort . L.map canonize

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
hananSolution p = makeSolution todo p
  where segs = filterAlwaysIn p $ candidates
        vias = concatMap segments $ getRoutedVias3D p
        candidates = filterObstacles p . hananSegs . points3d $ p
        startingSet = segs ++ vias
        todo = S.toList $
          S.fromList candidates `S.difference` S.fromList startingSet

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


account segs = rem
  where
    pts = L.concatMap (\(a, b) -> [a, b]) $ rmdups segs
    count = M.fromListWith (+) (zip pts (repeat 1))
    rempts = S.fromList $ L.map fst $ L.filter (\(p, c) -> c > 1) $ M.toList count
    rem = L.filter (\(a, b) -> S.member a rempts && S.member b rempts) segs

 {-

trimm :: [Point3D] -> [Segment] -> [Segment]
trimm pt segs = undefined
  where pts = S.fromList pt
        filter (\(a, b) -> S.member a pts && S.member a pts) segs


  -}
