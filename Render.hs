{-# LANGUAGE ScopedTypeVariables #-}

module Render
  ( allTriangles
  , boundingTriangles
  , boundingTrianglesSequence
  )
  where

import Data.List (find)
import Data.Maybe (isJust, fromJust)

import Solids
import Vectors
import ElaborateSolid


allTriangles :: Solid p -> [(p, p, p)]
allTriangles (Solid ps) =
  map (\[a, b, c] -> (a, b, c)) $ choose 3 ps


boundingTriangles :: (Floating a, Ord a) =>
  Solid (Vec3 a) -> [(Vec3 a, Vec3 a, Vec3 a)]
boundingTriangles = last . boundingTrianglesSequence

boundingTrianglesSequence :: (Floating a, Ord a) =>
  Solid (Vec3 a) -> [[(Vec3 a, Vec3 a, Vec3 a)]]
boundingTrianglesSequence =
  map extractTriangles . esSequence

esSequence :: forall a. (Floating a, Ord a) =>
  Solid (Vec3 a) -> [ElaborateSolid (Vec3 a) (Vec3 a, a)]
esSequence = esSequence' innerPoint mkPlane planeTest
  where
    mkPlane (a, b, c) =
      let normal = planeNormal a b c
      in (normal, normal `dot3d` a)
    planeTest :: (Vec3 a, a) -> Vec3 a -> Bool
    planeTest (normal, value) a = normal `dot3d` a <= value
    innerPoint (a, b, c, d) =
      if tetrahedronVolume a b c d > 10**(-5)
      then Just $ (1/4) `smult3d` foldl1 plus3d [a, b, c, d]
      else Nothing

esSequence' :: ((p, p, p, p) -> Maybe p)
  -> ((p, p, p) -> plane) -> (plane -> p -> Bool)
  -> Solid p -> [ElaborateSolid p plane]
esSequence' innerPoint mkPlane planeTest (Solid allPoints) =
  case mStartPoints of
    Nothing -> [emptyES]
    Just ([a, b, c, d], ps) ->
      myIterate
        (addPoint mkPlane planeTest)
        (tetrahedronES mkPlane planeTest
           (fromJust $ innerPoint (a, b, c, d)) a b c d)
        ps
    Just _ -> error "got non-length-4 list from findQuadrupel"
  where
    mStartPoints = findQuadruple
      (\[a, b, c, d] -> isJust (innerPoint (a, b, c, d)))
      allPoints

findQuadruple :: ([p] -> Bool)
  -> [p] -> Maybe ([p], [p])
findQuadruple t = find (t . fst) . choose' 4

myIterate :: (a -> b -> a) -> a -> [b] -> [a]
myIterate _ a [] = [a]
myIterate f a (b:bs) = a : myIterate f (f a b) bs

