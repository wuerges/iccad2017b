{-# LANGUAGE OverloadedStrings #-}

module Shapes where


import Data.ByteString

-- Data types for input

data Point = P (Int, Int)
  deriving Show
data Rect = R Point Point
  deriving Show

origin = P (0, 0)
dummyR = R origin origin

newtype Layer = Layer ByteString
  deriving Show
data Shape = Shape Layer Rect
  deriving Show
data Via = Via Layer Point
  deriving Show
data Obstacle = Obstacle Layer Rect
  deriving Show


data Problem = Problem
  { viaCost   :: Int
  , spacing   :: Int
  , boundary  :: Rect
  , layers    :: Int
  , routedShapes    :: [Shape]
  , routedVias      :: [Via]
  , obstacles :: [Obstacle]
  }
  deriving Show

--data VLine :: VLine Layer Rect
--data HLine :: VLine Layer Rect

data Solution = Solution
  { vlines :: [Shape]
  , hlines :: [Shape]
  , vias :: [Via]
  }
  deriving Show
