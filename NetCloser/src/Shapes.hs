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

getObstacles p = L.filter isObstacle (pelements p)
  where isObstacle (_, Obstacle _) = True
        isObstacle _ = False

getShapes p = L.filter isShape (pelements p)
  where isShape (_, Shape _) = True
        isShape _ = False


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

exampleProblem2 = Problem
  { viaCost = 20
  , spacing = 5
  , boundary = R (P (0,0)) (P (1000,1000))
  , metalLayers = 1
  , pelements = [ (LayerN 1, Shape (R (P (50,100)) (P (250,150))))
                , (LayerN 1, Shape (R (P (600,20)) (P (750,140))))
                , (LayerN 1, Shape (R (P (50,850)) (P (250,900))))
                , (LayerN 1, Shape (R (P (10,800)) (P (500,995))))
                , (LayerN 1, Obstacle (R (P (350,300)) (P (650,750))))
                , (LayerN 1, Obstacle (R (P (50,350)) (P (650,650))))
                ]
  , pvias = [ ]
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


elementCost (Hline r) = hipothenuse r
elementCost (Vline r) = hipothenuse r

solutionCost p s =
  elementCostT + viaCostT
  where
    elementCostT = sum (L.map (elementCost . snd) (selements s))
    viaCostT = viaCost p * L.length (svias s)

make3D p (LayerN l
         , Shape (R (P (x, y)) (P (x', y')))) =
           R3 { a = P3 x y (l * viaCost p)
              , b = P3 x' y' (l * viaCost p) }

make3D p (LayerN l
         , Obstacle (R (P (x, y)) (P (x', y')))) =
           R3 { a = P3 (min x x' - s) (min y y' - s) (l * vc)
              , b = P3 (max x x' + s) (max y y' + s) (l * vc) }
          where s = spacing p
                vc = viaCost p

make3D p (LayerN l
         , Via (P (x, y))) =
           R3 { a = P3 x y (l * vc)
              , b = P3 x y ((l+1) * vc) }
          where vc = viaCost p

make3D p (l, Hline r) = make3D p (l, Shape r)
make3D p (l, Vline r) = make3D p (l, Shape r)
make3D p (l, AddedVia pt) = make3D p (l, Via pt)


{-
segments3d _ (LayerN l

segments3d _ (_, Obstacle _) = []

segments3d p (LayerN l, Via (P (x, y))) =
  [ (P3 x y (l * spacing p), P3 x y ((l+1) * spacing p)) ]

-}

