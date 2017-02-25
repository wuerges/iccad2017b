module Geometry where

import Data.List as L
import Control.Arrow

data Point = P (Int, Int)
  deriving (Eq, Ord, Show)
data Rect = R Point Point
  deriving (Eq, Ord, Show)
