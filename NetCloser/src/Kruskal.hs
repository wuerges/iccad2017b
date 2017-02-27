module Kruskal ( runKruskallMST ) where

import Geometry

import Data.Equivalence.Monad as E
import Data.Map as M
import Control.Monad.State
import Control.Monad.Trans
import Data.Functor.Identity

type Edge = (Point3D, Point3D)
type Kruskal t a = StateT [Edge] (EquivM' t Point3D) a

{-| Equates all nodes in the initial shapes
   as members of its own subtrees.
   -}
kruskalMSTInit :: [[Point3D]] -> Kruskal t ()
kruskalMSTInit fs =
  mapM_ E.equateAll fs

step :: Edge -> Kruskal t ()
step (p1, p2) = do
  b <- equivalent p1 p2
  unless  b $ do
    equate p1 p2
    modify ((p1, p2):)

kruskalMST :: [Edge] -> Kruskal t [Edge]
kruskalMST start = do
  mapM_ step start
  get

runKruskallMST :: [[Point3D]] -> [Edge] -> [Edge]
runKruskallMST ps es = runEquivM' $ flip execStateT [] $ do
  kruskalMSTInit ps
  kruskalMST es


