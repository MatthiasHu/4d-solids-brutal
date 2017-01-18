module Permutations where


choose :: Int -> [a] -> [[a]]
choose n l = map fst $ choose' n l

choose' :: Int -> [a] -> [([a], [a])]
choose' 0 xs = [([], xs)]
choose' _ [] = []
choose' n (x:xs) =
  map (\(chosen, rest) -> (x:chosen, rest)) (choose' (n-1) xs)
  ++ map (\(chosen, rest) -> (chosen, x:rest)) (choose' n xs)
