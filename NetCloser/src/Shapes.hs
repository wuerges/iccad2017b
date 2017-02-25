{-# LANGUAGE OverloadedStrings #-}

module Shapes where

import Geometry

import Data.ByteString
import Data.List as L
import Control.Arrow
import Data.Maybe

-- Data types for input

origin = P (0, 0)
dummyR = R origin origin

newtype LayerN = LayerN Int
  deriving (Eq, Ord, Show)

data Shape = Shape Rect
           | Obstacle Rect
           | Via Point
           | Hline Rect
           | Vline Rect
           | AddedVia Point
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

exampleProblem = Problem
  { viaCost = 20
  , spacing = 5
  , boundary = R (P (0,0)) (P (1000,1000))
  , metalLayers = 2
  , pelements = [ (LayerN 1, Shape (R (P (50,100)) (P (250,150))))
                , (LayerN 1, Shape (R (P (600,20)) (P (750,140))))
                , (LayerN 1, Shape (R (P (50,850)) (P (250,900))))
                , (LayerN 1, Shape (R (P (10,800)) (P (500,995))))
                , (LayerN 2, Shape (R (P (75,20)) (P (200,750))))
                , (LayerN 2, Shape (R (P (375,100)) (P (575,600))))
                , (LayerN 2, Shape (R (P (475,20)) (P (670,450))))
                , (LayerN 1, Obstacle (R (P (350,300)) (P (650,750))))
                , (LayerN 1, Obstacle (R (P (50,350)) (P (650,650))))
                , (LayerN 2, Obstacle (R (P (350,700)) (P (950,800))))
                ]
  , pvias = [ (LayerN 1, Via (P (175,125))) ]
  }



exampleSolution = Solution
  { selements = [ (LayerN 1, Vline $ R (P (700, 140)) (P (700, 550) ))
                , (LayerN 2, Hline $ R (P (575, 550)) (P (700, 550) ))
                , (LayerN 1, Hline $ R (P (500, 850)) (P (700, 850) ))
                , (LayerN 1, Vline $ R (P (700, 550)) (P (700, 850) ))
                , (LayerN 2, Hline $ R (P (200, 150)) (P (375, 150) ))
                ]
  , svias = [ (LayerN 1, AddedVia $ P (700, 550)) ]
  , sMetalLayers = 2
  }
