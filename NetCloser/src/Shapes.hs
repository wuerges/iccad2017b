{-# LANGUAGE OverloadedStrings #-}

module Shapes where


import Data.ByteString
import Data.List as L
import Control.Arrow
import Data.Maybe

-- Data types for input

data Point = P (Int, Int)
  deriving (Eq, Ord, Show)
data Rect = R Point Point
  deriving (Eq, Ord, Show)

origin = P (0, 0)
dummyR = R origin origin

newtype LayerN = LayerN Int
  deriving (Eq, Ord, Show)

data Shape = Shape Rect
           | Obstacle Rect
           | Hline Rect
           | Vline Rect
           | Via Point
           deriving (Eq, Ord, Show)

data Problem = Problem
  { viaCost   :: Int
  , spacing   :: Int
  , boundary  :: Rect
  , metalLayers :: Int
  , pelements :: [(LayerN, Shape)]
  , pvias :: [(LayerN, Shape)]
  }
  deriving Show

next (LayerN i) = LayerN (i+1)

groupLayers :: Ord a
            => [(LayerN, a)]
            -> [(LayerN, [a])]
groupLayers es = catMaybes $ L.map tagG groups
  where
    groups = L.groupBy (\a b -> fst a == fst b) $ L.sort es
    tagG :: [(LayerN, a)] -> Maybe (LayerN, [a])
    tagG []           = Nothing
    tagG ((ln, e):rs) = Just (ln, e:L.map snd rs)


stackVias :: [(LayerN, Shape)] -> [(LayerN, Shape)]
stackVias vs = vs ++ (L.map (first next) vs)


groupEverything :: [(LayerN, Shape)]
                -> [(LayerN, Shape)]
                -> [(LayerN, [Shape])]
groupEverything es vs =
  groupLayers (es ++ stackVias vs)


data Solution = Solution
  { selements :: [(LayerN, Shape)]
  , svias :: [(LayerN, Shape)]
  , sMetalLayers :: Int
  }


