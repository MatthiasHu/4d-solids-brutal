module Render where

import qualified Data.Set as Set
import qualified Data.IntMap as IMap

import Solids


allTriangles :: Solid p -> [(p, p, p)]
allTriangles (Solid ps) =
  map (\[a, b, c] -> (a, b, c)) $ choose 3 ps

choose :: Int -> [a] -> [[a]]
choose 0 _ = [[]]
choose n [] = []
choose n (x:xs) = map (x:) (choose (n-1) xs) ++ choose n xs

choose' :: Int -> [a] -> [([a], [a])]
choose' 0 xs = [([], xs)]
choose' n [] = []
choose' n (x:xs) =
  map (\(chosen, rest) -> (x:chosen, rest)) (choose' (n-1) xs)
  ++ map (\(chosen, rest) -> (chosen, x:rest)) (choose' n xs)


boundingTriangles :: ((p, p, p) -> plane) -> (plane -> p -> Bool)
  -> Solid p -> [(p, p, p)]
boundingTriangles mkPlane planeTest (Solid allPoints) =
  undefined $ foldl
    (addPoint mkPlane planeTest)
    (initialES mkPlane a b c d)
    ps
  where
    a:b:c:d:ps = allPoints

-- a 3d solid with a triangulation of the surface
data ElaborateSolid p plane = ElaborateSolid
  { vertices :: IMap.IntMap p
  , edges :: [Edge Int plane]
  , faces :: [Face Int plane]
  }

type Edge p plane = ((p, p), (plane, plane))
type Face p plane = ((p, p, p), plane)

initialES :: ((p, p, p) -> plane)
  -> p -> p -> p -> p -> ElaborateSolid p plane
initialES mkPlane a0 b0 c0 d0 = ElaborateSolid
  { vertices = verts
  , edges = map (\ ([a, b], [c, d]) -> undefined) $ choose' 2 vertIds
  , faces = undefined
  }
  where
    verts = IMap.fromList (zip vertIds [a0, b0, c0, d0])
    vertIds = [0..3]

addPoint :: ((p, p, p) -> plane) -> (plane -> p -> Bool)
  -> ElaborateSolid p plane -> p -> ElaborateSolid p plane
addPoint = undefined
