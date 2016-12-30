{-# LANGUAGE ScopedTypeVariables #-}

module Render where

import qualified Data.IntSet as ISet
import qualified Data.Set as Set
import qualified Data.IntMap as IMap
import qualified Data.Map.Strict as Map
import Data.List (mapAccumL)

import Solids
import Vectors


allTriangles :: Solid p -> [(p, p, p)]
allTriangles (Solid ps) =
  map (\[a, b, c] -> (a, b, c)) $ choose 3 ps

choose :: Int -> [a] -> [[a]]
choose n l = map fst $ choose' n l

choose' :: Int -> [a] -> [([a], [a])]
choose' 0 xs = [([], xs)]
choose' n [] = []
choose' n (x:xs) =
  map (\(chosen, rest) -> (x:chosen, rest)) (choose' (n-1) xs)
  ++ map (\(chosen, rest) -> (chosen, x:rest)) (choose' n xs)


boundingTriangles :: forall a. (Floating a, Ord a) =>
  Solid (Vec3 a) -> [(Vec3 a, Vec3 a, Vec3 a)]
boundingTriangles = last . boundingTrianglesSequence

boundingTrianglesSequence :: forall a. (Floating a, Ord a) =>
  Solid (Vec3 a) -> [[(Vec3 a, Vec3 a, Vec3 a)]]
boundingTrianglesSequence =
  map extractTriangles . esSequence

esSequence :: forall a. (Floating a, Ord a) =>
  Solid (Vec3 a) -> [ElaborateSolid (Vec3 a) (Vec3 a, a)]
esSequence = esSequence' mkPlane planeTest
  where
    mkPlane (a, b, c) =
      let normal = planeNormal a b c
      in (normal, normal `dot3d` a)
    planeTest :: (Vec3 a, a) -> Vec3 a -> Bool
    planeTest (normal, value) a = normal `dot3d` a <= value

esSequence' :: ((p, p, p) -> plane) -> (plane -> p -> Bool)
  -> Solid p -> [ElaborateSolid p plane]
esSequence' mkPlane planeTest (Solid allPoints) =
  myIterate
    (addPoint mkPlane planeTest)
    (initialES mkPlane planeTest a b c d)
    ps
  where
    a:b:c:d:ps = allPoints
    elaborateSolid = foldl
      (addPoint mkPlane planeTest)
      (initialES mkPlane planeTest a b c d)
      ps

myIterate :: (a -> b -> a) -> a -> [b] -> [a]
myIterate f a [] = [a]
myIterate f a (b:bs) = a : myIterate f (f a b) bs

extractTriangles :: ElaborateSolid p plane -> [(p, p, p)]
extractTriangles es =
  map (toTriple . map (vertices es IMap.!) . faceIdToList)
  . Map.keys . faces
  $ es

-- delete this?
boundingTriangles' :: ((p, p, p) -> plane) -> (plane -> p -> Bool)
  -> Solid p -> [(p, p, p)]
boundingTriangles' mkPlane planeTest (Solid allPoints) =
  map (toTriple . map (vertices elaborateSolid IMap.!) . faceIdToList)
  . Map.keys . faces
  $ elaborateSolid
  where
    a:b:c:d:ps = allPoints
    elaborateSolid = foldl
      (addPoint mkPlane planeTest)
      (initialES mkPlane planeTest a b c d)
      ps

-- a 3d solid with a triangulation of the surface
data ElaborateSolid p plane = ElaborateSolid
  { vertices :: IMap.IntMap p
  , faces :: Map.Map FaceId (FaceData plane)
  , nextVertId :: VertId
  }

type VertId = IMap.Key

data EdgeId = EdgeIdRaw VertId VertId
  deriving (Show, Eq, Ord)

mkEdgeId :: VertId -> VertId -> EdgeId
mkEdgeId a b | a <= b     = EdgeIdRaw a b
             | otherwise  = EdgeIdRaw b a

edgeIdToList :: EdgeId -> [VertId]
edgeIdToList (EdgeIdRaw a b) = [a, b]

data FaceId = FaceIdRaw VertId VertId VertId
  deriving (Show, Eq, Ord)

mkFaceId :: VertId -> VertId -> VertId -> FaceId
mkFaceId a b c | a <= b && b <= c = FaceIdRaw a b c
               | a > b            = mkFaceId b a c
               | otherwise        = mkFaceId a c b

faceIdToList :: FaceId -> [VertId]
faceIdToList (FaceIdRaw a b c) = [a, b, c]

faceToEdges :: FaceId -> [EdgeId]
faceToEdges = map fst . faceToEdges'

faceToEdges' :: FaceId -> [(EdgeId, VertId)]
faceToEdges' (FaceIdRaw a b c) =
  [ (mkEdgeId a b, c)
  , (mkEdgeId a c, b)
  , (mkEdgeId b c, a)
  ]

type EdgeData = (FaceId, FaceId)
type FaceData plane = (plane, Bool)

toTriple :: [a] -> (a, a, a)
toTriple [a, b, c] = (a, b, c)
toTriple _ = error "toTriple: not a length 3 list"

fromPair :: (a, a) -> [a]
fromPair (a, b) = [a, b]

initialES :: ((p, p, p) -> plane) -> (plane -> p -> Bool)
  -> p -> p -> p -> p -> ElaborateSolid p plane
initialES mkPlane planeTest a0 b0 c0 d0 = ElaborateSolid
  { vertices = verts
  , faces = Map.fromList
      . map ( \ ([a, b, c], [d]) ->
        let pl = mkPlane . toTriple . map (verts IMap.!) $ [a, b, c]
        in (mkFaceId a b c, (pl, planeTest pl (verts IMap.! d)))
        )
      $ choose' 3 vertIds
  , nextVertId = length vertIds
  }
  where
    verts = IMap.fromList $ zip vertIds [a0, b0, c0, d0]
    vertIds = [0..3]

addPoint :: forall p plane.
  ((p, p, p) -> plane) -> (plane -> p -> Bool)
  -> ElaborateSolid p plane -> p -> ElaborateSolid p plane
addPoint mkPlane planeTest oldSolid newPoint =
  ElaborateSolid newVerts newFaces (nextVertId oldSolid +1)
  where
    lookupVert :: VertId -> p
    lookupVert k
      | k == newVertId = newPoint
      | otherwise                = vertices oldSolid IMap.! k
    lookupFace k = faces oldSolid Map.! k
    newVertId = nextVertId oldSolid

    edgeSetToVertSet :: Set.Set EdgeId -> ISet.IntSet
    edgeSetToVertSet = ISet.fromList . concatMap edgeIdToList . Set.toList

    oldVerts = vertices oldSolid
    newVerts = IMap.union keepVerts maybeNewVert
    maybeNewVert = if Map.null dropFaces
      then IMap.empty
      else IMap.singleton newVertId newPoint
    keepVerts :: IMap.IntMap p
    keepVerts =
      IMap.fromSet (vertices oldSolid IMap.!)
      . ISet.fromList . concatMap faceIdToList
      $ Map.keys keepFaces
    keepFaces :: Map.Map FaceId (FaceData plane)
    dropFaces :: Map.Map FaceId (FaceData plane)
    (keepFaces, dropFaces) = Map.partition
      (\(pl, inside) -> planeTest pl newPoint == inside)
      (faces oldSolid)
    criticalEdges :: Map.Map EdgeId VertId
    criticalEdges = Map.filterWithKey
      (\k v -> Set.member k positiveCriticalEdges)
      (Map.fromList . concatMap faceToEdges' . Map.keys $ dropFaces)
    positiveCriticalEdges :: Set.Set EdgeId
    positiveCriticalEdges = Set.fromList . concatMap faceToEdges
      $ Map.keys keepFaces
--    edgesOfFaces :: Map.Map FaceId a -> Set.Set EdgeId
--    edgesOfFaces = Set.fromList . concatMap faceToEdges . Map.keys
    newFaces :: Map.Map FaceId (FaceData plane)
    newFaces = Map.union keepFaces . Map.fromList
      . map (\((EdgeIdRaw a b), othervert) ->
          ( mkFaceId a b newVertId
          , let pl = mkPlane
                  ( oldVerts IMap.! a
                  , oldVerts IMap.! b
                  , newPoint )
            in (pl, planeTest pl (oldVerts IMap.! othervert))))
      . Map.toList $ criticalEdges
