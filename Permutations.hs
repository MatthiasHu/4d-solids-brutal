module Permutations where


choose :: Int -> [a] -> [[a]]
choose n l = map fst $ choose' n l

choose' :: Int -> [a] -> [([a], [a])]
choose' 0 xs = [([], xs)]
choose' _ [] = []
choose' n (x:xs) =
  map (\(chosen, rest) -> (x:chosen, rest)) (choose' (n-1) xs)
  ++ map (\(chosen, rest) -> (chosen, x:rest)) (choose' n xs)


selections :: [a] -> [(a, [a])]
selections [] = []
selections (x:xs) = (x, xs)
  : [ (y, x:ys) | (y, ys) <- selections xs ]

-- same as in Data.List (except for order)
permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations xs = do
  (y, ys) <- selections xs
  ys' <- permutations ys
  return (y : ys')


data Parity = Even | Odd
  deriving (Eq, Ord, Show)

instance Monoid Parity where
  mempty = Even
  mappend Even Even = Even
  mappend Odd  Odd  = Even
  mappend _    _    = Odd

other :: Parity -> Parity
other Even = Odd
other Odd  = Even

selectionsPar :: [a] -> [(Parity, a, [a])]
selectionsPar [] = []
selectionsPar (x:xs) = (Even, x, xs) :
  [ (other par, y, x:ys) | (par, y, ys) <- selectionsPar xs ]

permutationsPar :: [a] -> [(Parity, [a])]
permutationsPar [] = [(Even, [])]
permutationsPar xs = do
  (par1, y, ys) <- selectionsPar xs
  (par2, ys') <- permutationsPar ys
  return (mappend par1 par2, y:ys')

evenPermutations :: [a] -> [[a]]
evenPermutations = map snd . filter ((==Even) . fst) . permutationsPar
