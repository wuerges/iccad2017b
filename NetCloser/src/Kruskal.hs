module Kruskal ( runKruskalMST ) where

import Geometry

import Data.Equivalence.Monad as E
import Data.Map as M
import Control.Monad.State
import Control.Monad.Trans
import Data.Functor.Identity
import Data.List as L

type Edge = (Point3D, Point3D)
type Kruskal t a = StateT [Edge] (EquivM' t Point3D) a

{-| Equates all nodes in the initial shapes
   as members of its own subtrees.
   -}
kruskalMSTInit :: [[Point3D]] -> Kruskal t ()
kruskalMSTInit fs =
  mapM_ E.equateAll fs

-- | Performs one iteration of Kruskal MST
step :: Edge -> Kruskal t ()
step (p1, p2) = do
  b <- equivalent p1 p2
  unless  b $ do
    equate p1 p2
    modify ((p1, p2):)

-- | Performs all iterations of Kruskal MST
kruskalMST :: [Edge] -> Kruskal t [Edge]
kruskalMST start = do
  mapM_ step start
  get

-- | The 'runKruskalMST' function
-- performs Kruskal minimum spanning tree.
-- The first parameter is a list of lists of points, where
-- every list is a Tree.
-- The second parameter is the list of edges to check.
runKruskalMST :: [[Point3D]] -> [Edge] -> [Edge]
runKruskalMST ps es = runEquivM' $ flip execStateT [] $ do
  kruskalMSTInit ps
  kruskalMST (L.sortOn distance es)


