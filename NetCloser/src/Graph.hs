module Graph where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Basic
import Data.Graph.Inductive.NodeMap
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.SP
import Data.Graph.Inductive.Query.MST
import Debug.Trace

import Hanan
import Shapes
import Geometry
import Kruskal
import Data.Maybe
import Data.List as L


-- | The type G describes
-- a graph with nodes containing
-- 3d points and edges containing a cost.
-- The cost is zero if the edge is routed.
-- The cost is the distance between
-- the nodes if the edge is not routed.
type G = Gr Point3D Int
type EdgeG = LEdge Int


-- | Incorporates a shape into the Graph,
-- creating a new graph.
incorporate :: Rect3D -> G -> G
incorporate r g =
  foldr (incorporateEdge r) g $ edges g


-- | Incorporates all the shapes of the
-- problem into the graph.
incorporateShapes :: Problem -> G -> G
incorporateShapes p g =
  --incorporate (rects !! 6) g
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
embedSeg (a, b) g =
  insMapEdge m (a, b, distance (a, b)) g'
  where
    g' = embedPt b $ embedPt a g
    m = fromGraph g'

        --(g', m', [na, nb]) = insMapNodes m [a, b] g

-- | embeds a point in the graph
embedPt p g
  | gelem n g = g
  | otherwise = insNode (n, l) g
  where m = fromGraph g
        (n, l) = mkNode_ m p


-- | Initializes the Hanan grid
-- without the obstacles
initHanan :: Problem -> G
initHanan p = g'
--foldr embedSeg empty candidates
  where
    candidates = filterObstacles p . hananSegs . points3d $ p
    pts = concatMap (\(a, b) -> [a, b]) candidates
    (g0, m0, _) = insMapNodes new pts empty
    labeled_candidates = [(a, b, distance (a, b)) | (a, b) <- candidates]
    g' = insMapEdges m0 labeled_candidates g0


-- | Incorporates an edge into the graph
incorporateEdge r (n1, n2) g
  | isInsideR r (n1, n2) g =
    insEdge (n1, n2, 0) $ delEdge (n2, n1) $ delEdge (n1, n2) g
  | otherwise = g

-- | Checks if an edge is inside the rectangle
isInsideR r (n1, n2) g = r1 && r2
  where mp1 = lab g n1
        mp2 = lab g n2
        r1 = maybe False (\p -> collidesP r p) mp1
        r2 = maybe False (\p -> collidesP r p) mp2


-- | gets a representative node for r in the  garph
representative g r =
  L.find (\(n, p) -> collidesP r p) $ labNodes g

segmentsGraph g = catMaybes $ map segEdge $ edges g
  where
    segEdge (n1, n2) = case (lab g n1, lab g n2) of
                         (Just a, Just b) -> Just (a, b)
                         _ -> Nothing

makeSolutionG :: Problem -> G -> Solution
makeSolutionG p g = --traceShow (L.length $ edges g, L.length segs) $
  Solution
    { selements = filter (not . isVia) shapes
    , svias = filter isVia shapes
    , sMetalLayers = metalLayers p }
  where
    vc = viaCost p
    fixLayer (LayerN n, x) = (LayerN $ n `div` vc, x)
    segs = segmentsGraph g
    shapes =  map (fixLayer . segmentToShape) segs
    isVia (_, AddedVia _) = True
    isVia (_, _) = False

makeSimpleSolution p = makeSolutionG p $
  removeZeroCost $ cleanup g''
  where g = incorporateShapes p $ incorporateVias p $ initHanan p
        mst_nodes = rmdup $ concatMap (\(a, b, _) -> [a, b]) k
        --g' = traceShow k $ subgraph mst_nodes g
        rs = map fst $ representatives p g
        g' = inducedShortestPaths rs g
        k = runKruskalMST2 $ labEdges g'
        g'' = insEdges k . insNodes (labNodes g) $ empty
        --mst = subgraph mst_nodes g'
        --mst = g'
        --mst_nodes :: [Node]
        --mst_nodes = map fst $ concatMap unLPath $ msTree g'
        --mst_nodes = map fst $ unLPath $ head $ msTree g'

sumCostEdges = sum . map edgeLabel


representatives p g =
  catMaybes $ map (representative g) shapes
    where shapes = map (make3D p) $ getShapes p

inducedShortestPaths :: [Node] -> G -> G
inducedShortestPaths ns g = subgraph rs g'
  where
    g' = undir g
    rps = rmdup [(min a b, max a b) | a <- ns, b <- ns, a /= b]
    rs = rmdup $ concatMap sp' $ rps
    sp' (a, b) = sp a b g'


removeZeroCost :: G -> G
removeZeroCost g = g'
  where
    k = filter (\e -> edgeLabel e /= 0) $ labEdges g
    g' = insEdges k . insNodes (labNodes g) $ empty


cleanup g | order g == order g' = g
          | otherwise  = cleanup g'
  where
    g' = nfilter (\n -> deg g n > 1) g
--cleanup g = subgraph nrem g
--  where nrem = [n | n <- nodes g, deg g n > 1]

