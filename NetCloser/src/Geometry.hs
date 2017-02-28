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

distance (P3 x y z, P3 x' y' z') =
  abs (x - x') + abs (y - y') + abs (z - z')

