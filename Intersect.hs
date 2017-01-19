module Intersect where

import Permutations
import Solids
import Vectors


-- abstract intersection with a (hyper-) plane
intersect :: (p -> Bool) -> Solid p -> Solid (p, p)
intersect t = intersectEdges t . allEdges

intersectEdges :: (p -> Bool) -> [(p, p)] -> Solid (p, p)
intersectEdges t = Solid . filter (\(x, y) -> t x /= t y)

allEdges :: Solid p -> [(p, p)]
allEdges (Solid ps) = map (\[a, b] -> (a, b)) . choose 2 $ ps

-- all (potential) edges of squared length <= dSq
shortEdges4d :: (Num a, Ord a) =>
  a -> Solid (Vec4 a) -> [(Vec4 a, Vec4 a)]
shortEdges4d dSq = filter ((<=dSq) . uncurry distSq4d) . allEdges

edges4dRegular :: (Fractional a, Ord a) =>
  Solid (Vec4 a) -> [(Vec4 a, Vec4 a)]
edges4dRegular s@(Solid ps) = shortEdges4d (1.1*edgeLengthSq) s
  where
    (p0:ps') = ps
    edgeLengthSq = minimum . map (distSq4d p0) $ ps'


-- intersect 4d solid with x-y-z-hyperplane to get 3d solid in 3d space
intersectXYZ :: (Fractional s, Ord s) => Solid (Vec4 s) -> Solid (Vec3 s)
intersectXYZ = fmap interpolateXYZ . intersect ((<0) . coord4dw)

-- same as intersectXYZ, but assuming that the 4d solid is regular
-- (all edges of same length) and the edge length is the distance
-- of the closest vertices
intersectXYZRegular :: (Fractional s, Ord s) =>
  Solid (Vec4 s) -> Solid (Vec3 s)
intersectXYZRegular =
    fmap interpolateXYZ
  . intersectEdges ((<0) . coord4dw)
  . edges4dRegular

interpolateXYZ :: (Fractional a) => (Vec4 a, Vec4 a) -> Vec3 a
interpolateXYZ (a, b) = plus3d
  ( (coord4dw b / dw) `smult3d` coord4dxyz a )
  ((-coord4dw a / dw) `smult3d` coord4dxyz b )
  where dw = coord4dw b - coord4dw a
