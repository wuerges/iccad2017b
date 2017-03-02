module Graph where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.NodeMap
import Data.Graph.Inductive.PatriciaTree

import Hanan
import Shapes
import Geometry
import Data.Maybe


-- | The type G describes
-- a graph with nodes containing
-- 3d points and edges containing a cost.
-- The cost is zero if the edge is routed.
-- The cost is the distance between
-- the nodes if the edge is not routed.
type G = Gr Point3D Int


-- | Incorporates a shape into the Graph,
-- creating a new graph.
incorporate :: Rect3D -> G -> G
incorporate r g =
  foldr (incorporateEdge r) g $ edges g


-- | Incorporates all the shapes of the
-- problem into the graph.
incorporateShapes :: Problem -> G -> G
incorporateShapes p g =
  foldr incorporate g rects
  where
    shapes = getShapes p
    rects = map (make3D p) shapes

-- | Incorporates all pre routed vias
-- in the graph
incorporateVias :: Problem -> G -> G
incorporateVias p g  =
  foldr incorporate g rects
    where
      vias = pvias p
      rects = map (make3D p) vias


-- | inserts a segmemnt into the graph
-- Creates new nodes if needed, otherwise
-- use the ones already inside
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


-- | Incorporates an edge into the graph
incorporateEdge r (n1, n2) g
  | isInsideR r (n1, n2) g =
    insEdge (n1, n2, 0) $ delEdge (n1, n2) g
  | otherwise = g

-- | Checks if an edge is inside the rectangle
isInsideR r (n1, n2) g = r1 && r2
  where mp1 = lab g n1
        mp2 = lab g n2
        r1 = maybe False (\p -> collidesP r p) mp1
        r2 = maybe False (\p -> collidesP r p) mp2



