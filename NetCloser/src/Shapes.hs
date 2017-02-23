{-# LANGUAGE OverloadedStrings #-}

module Shapes where


import Data.Map as M
import Data.ByteString
import Data.List as L

-- Data types for input

data Point = P (Int, Int)
  deriving (Eq, Ord, Show)
data Rect = R Point Point
  deriving (Eq, Ord, Show)

origin = P (0, 0)
dummyR = R origin origin

newtype LayerN = LayerN Int
  deriving (Eq, Ord, Show)

newtype Shape = Shape Rect
  deriving Show

data Via = Via Point
  deriving Show

data Obstacle = Obstacle Rect
  deriving Show

data Layer = Layer [Shape] [Via] [Obstacle]
  deriving Show

data Problem = Problem
  { viaCost   :: Int
  , spacing   :: Int
  , boundary  :: Rect
  , metalLayers :: Int
  , routedShapes :: Map LayerN [Shape]
  , routedVias   :: Map LayerN [Via]
  , obstacles    :: Map LayerN [Obstacle]
  }
  deriving Show

next (LayerN i) = LayerN (i+1)

getLayer :: Problem -> LayerN -> Layer
getLayer p l@(LayerN i) =
  Layer (f l $ routedShapes p)
        ((f l $ routedVias p) ++ (f (next l) $ routedVias p))
        (f l $ obstacles p)
  where f ln = findWithDefault [] ln


splitLayers :: Problem -> [(LayerN, Layer)]
splitLayers p =
  L.map
    (\ln -> (ln, getLayer p ln) )
    [LayerN i | i <- [1..metalLayers p]]

data Solution = Solution
  { vlines :: [Shape]
  , hlines :: [Shape]
  , vias :: [Via]
  }
