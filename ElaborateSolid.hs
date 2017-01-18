{-# LANGUAGE ScopedTypeVariables #-}

module ElaborateSolid
  ( ElaborateSolid()
  , emptyES
  , tetrahedronES
  , addPoint
  , extractTriangles
  )
  where

import qualified Data.IntSet as ISet
import qualified Data.Set as Set
import qualified Data.IntMap as IMap
import qualified Data.Map.Strict as Map

import Permutations


-- a 3d solid with a triangulation of the surface
data ElaborateSolid p plane = ElaborateSolid
  { vertices :: IMap.IntMap p
  , faces :: Map.Map FaceId (FaceData plane)
  , nextVertId :: VertId
  , center :: p
  }

type VertId = IMap.Key

data EdgeId = EdgeIdRaw VertId VertId
  deriving (Show, Eq, Ord)

mkEdgeId :: VertId -> VertId -> EdgeId
mkEdgeId a b | a <= b     = EdgeIdRaw a b
             | otherwise  = EdgeIdRaw b a

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

type FaceData plane = (plane, Bool)

toTriple :: [a] -> (a, a, a)
toTriple [a, b, c] = (a, b, c)
toTriple _ = error "toTriple: not a length 3 list"

emptyES :: ElaborateSolid p plane
emptyES = ElaborateSolid IMap.empty Map.empty 0 undefined

tetrahedronES :: ((p, p, p) -> plane) -> (plane -> p -> Bool) -> p
  -> p -> p -> p -> p -> ElaborateSolid p plane
tetrahedronES mkPlane planeTest centerPoint a0 b0 c0 d0 = ElaborateSolid
  { vertices = verts
  , faces = Map.fromList
      . map ( \ ([a, b, c], [d]) ->
        let pl = mkPlane . toTriple . map (verts IMap.!) $ [a, b, c]
        in (mkFaceId a b c, (pl, planeTest pl (verts IMap.! d)))
        )
      $ choose' 3 vertIds
  , nextVertId = length vertIds
  , center = centerPoint
  }
  where
    verts = IMap.fromList $ zip vertIds [a0, b0, c0, d0]
    vertIds = [0..3]

addPoint :: forall p plane.
  ((p, p, p) -> plane) -> (plane -> p -> Bool)
  -> ElaborateSolid p plane -> p -> ElaborateSolid p plane
addPoint mkPlane planeTest oldSolid newPoint =
  ElaborateSolid
    newVerts
    newFaces
    (nextVertId oldSolid +1)
    (center oldSolid)
  where
    newVertId = nextVertId oldSolid
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
    criticalEdges :: Set.Set EdgeId
    criticalEdges = Set.intersection
      (edgesOfFaces keepFaces)
      (edgesOfFaces dropFaces)
    edgesOfFaces :: Map.Map FaceId a -> Set.Set EdgeId
    edgesOfFaces = Set.fromList . concatMap faceToEdges . Map.keys
    newFaces :: Map.Map FaceId (FaceData plane)
    newFaces = Map.union keepFaces . Map.fromList
      . map (\(EdgeIdRaw a b) ->
          ( mkFaceId a b newVertId
          , let pl = mkPlane
                  ( oldVerts IMap.! a
                  , oldVerts IMap.! b
                  , newPoint )
            in (pl, planeTest pl (center oldSolid))))
      . Set.toList $ criticalEdges

extractTriangles :: ElaborateSolid p plane -> [(p, p, p)]
extractTriangles es =
  map (toTriple . map (vertices es IMap.!) . faceIdToList)
  . Map.keys . faces
  $ es

