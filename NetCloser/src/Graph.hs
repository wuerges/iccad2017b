module Graph where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.NodeMap
import Data.Graph.Inductive.PatriciaTree

import Hanan
import Shapes
import Geometry


-- | The type G describes
-- a graph with nodes containing
-- 3d points and edges containing a cost.
-- The cost is zero if the edge is routed.
-- The cost is the distance between
-- the nodes if the edge is not routed.
type G = Gr Point3D Int


-- | Incorporates a shape into the Graph,
-- creating a new graph and
-- a representative node.
incorporate :: Shape -> G -> (G, Node)
incorporate = undefined

embedSeg (a, b) g = insEdge e g'
  where m = fromGraph g
        (g', m', [na, nb]) = insMapNodes m [a, b] g
        e = (fst na, fst nb, distance (a, b))


-- | Initializes the Hanan grid
-- without the obstacles
initHanan :: Problem -> G
initHanan p = foldr embedSeg empty candidates
  where
    candidates = filterObstacles p . hananSegs . points3d $ p



