
module Hanan where

import Geometry
import Shapes

points3dShape _ (LayerN l
  , Shape (R (P (x, y)) (P (x', y')))) =
  [ P3 x y l
  , P3 x y' l
  , P3 x' y l
  , P3 x' y' l ]

points3dShape _ (LayerN l, Via (P (x, y))) =
  [ P3 x y l
  , P3 x y (l+1) ]


points3dShape s (LayerN l
                , Obstacle
                          (R (P (x, y)) (P (x', y')))) =
                            [ P3 ox  oy  l
                            , P3 ox  oy' l
                            , P3 ox' oy  l
                            , P3 ox' oy' l ]
 where ox = x - s
       oy = y - s
       ox' = x' + s
       oy' = y' + s

points3dShape _ _            = error "This function should not be used here"

hananPs :: [Point3D] -> [Point3D]
hananPs ps =
  [ P3 px py pz
    | px <- map x ps, py <- map y ps, pz <- map z ps ]



points3d :: Problem -> [Point3D]
points3d p = map (setViaCost (viaCost p)) ps
  where
    ps = concatMap (points3dShape (spacing p)) (pelements p ++ pvias p)
    setViaCost vc (P3 x y z) = (P3 x y (z * vc))

hanan = hananPs . points3d




