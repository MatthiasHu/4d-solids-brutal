module Render where

import Solids


allTriangles :: Solid p -> [(p, p, p)]
allTriangles (Solid ps) =
  map (\[a, b, c] -> (a, b, c)) $ choose 3 ps

choose :: Int -> [a] -> [[a]]
choose 0 _ = [[]]
choose n [] = []
choose n (x:xs) = map (x:) (choose (n-1) xs) ++ choose n xs
