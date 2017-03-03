module Geometry where

import Data.List as L
import Control.Arrow

{-|
    The P constructor creates a 2D point inside a Metal Layer.
    The coordinates are X, Y.
    The X and Y axes are for points inside a Metal Layer.
-}
data Point = P (Int, Int)
  deriving (Eq, Ord, Show)

{-|
    The P3 constructor creates a 3D point.
    The coordinates are x, y and z.
    The x and y axis are for points inside a Metal Layer.
    The z stands for the Metal Layer.
-}
data Point3D = P3 { x :: Int, y ::  Int, z :: Int }
  deriving (Eq, Ord, Show)

{-|
   The R construction creates a 2D rectangle inside a Metal Layer.
   The rectangle is represented by 2 oposing corners.
   -}
data Rect = R Point Point
  deriving (Eq, Ord, Show)

data Rect3D = R3 { a :: Point3D, b :: Point3D }

distance (P3 x y z, P3 x' y' z') =
  abs (x - x') + abs (y - y') + abs (z - z')


collidesP (R3 (P3 x1 y1 z1) (P3 x2 y2 z2)) (P3 px py pz) =
  col z1 z2 pz && col x1 x2 px && col y1 y2 py
  where
    col a b c = (min a b) <= c && (max a b) >= c

collides r (R3 p1 p2) = collidesP r p1 || collidesP r p2

segmentInside r (p1, p2) = collidesP r p1 && collidesP r p2


hipothenuse (R (P (x1, y1)) (P (x2, y2))) =
  abs (x1 - x2) + abs (y1 - y2)

resize (R3 (P3 x1 y1 z1) (P3 x2 y2 z2)) n =
  R3 a b
    where
      minx = min x1 x2
      miny = min y1 y2
      maxx = max x1 x2
      maxy = max y1 y2
      a = P3 (minx-n) (miny-n) z1
      b = P3 (maxx+n) (maxy+n) z1



points s = [fst s, snd s]
