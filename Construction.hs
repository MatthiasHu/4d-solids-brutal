module Construction where

import Data.List (nub)

import Solids
import Vectors
import Permutations


point :: Solid [a]
point = Solid [[]]

prism :: a -> a -> Solid [a] -> Solid [a]
prism a b (Solid ps) = Solid $ map (a:) ps ++ map (b:) ps

prism' :: (Num a) => Solid [a] -> Solid [a]
prism' = prism (-1) 1

crossPolytope :: (Num a) => Int -> Solid [a]
crossPolytope 0 = Solid []
crossPolytope n = Solid $
  [ (-1) : replicate (n-1) 0
  ,   1  : replicate (n-1) 0 ]
  ++ map (0:) ps
  where Solid ps = crossPolytope (n-1)

simplex :: (Floating a) => Int -> Solid [a]
simplex 0 = Solid [[]]
simplex n = Solid $ tip : base
  where
    tip = (1: replicate (n-1) 0)
    Solid base = fmap ((baselevel:) . map (*basescale)) $ simplex (n-1)
    baselevel = -1 / fromIntegral n
    basescale = sqrt (1-baselevel^2)


cube :: (Num a) => Solid (Vec3 a)
cube = fmap fromList3d $ prism' . prism' . prism' $ point

hypercube, tesseract, cell8 :: (Floating a) => Solid (Vec4 a)
hypercube = fmap (normalize4d . fromList4d) $
  prism' . prism' . prism' . prism' $ point
tesseract = hypercube
cell8 = hypercube

octahedron :: (Num a) => Solid (Vec3 a)
octahedron = fmap fromList3d $ crossPolytope 3

cell16 :: (Num a) => Solid (Vec4 a)
cell16 = fmap fromList4d $ crossPolytope 4

hyperdiamond, cell24 :: (Floating a) => Solid (Vec4 a)
hyperdiamond = Solid . map normalize4d $ as ++ bs
  where
    Solid as = hypercube
    Solid bs = cell16
cell24 = hyperdiamond

simplex3, tetrahedron :: (Floating a) => Solid (Vec3 a)
simplex3 = fmap fromList3d $ simplex 3
tetrahedron = simplex3

simplex4, cell5 :: (Floating a) => Solid (Vec4 a)
simplex4 = fmap fromList4d $ simplex 4
cell5 = simplex4

-- coordinates of cell600 and cell120 taken from en.wikipedia.org

cell600 :: (Floating a) => Solid (Vec4 a)
cell600 = Solid $ as ++ bs
  where
    Solid as = hyperdiamond
    bs =
      map (normalize4d . fromList4d)
      . concatMap evenPermutations . choises $
      [ pm phi, pm 1, pm (1/phi), [0] ]
    pm = plusminus

cell120 :: (Floating a, Eq a) => Solid (Vec4 a)
cell120 =
  Solid . map (normalize4d . fromList4d) . concat $
  [ concatMap (nub . concatMap permutations . choises) $
    [ [[0], [0], pm 2, pm 2]
    , [pm 1, pm 1, pm 1, pm (sqrt 5)]
    , [pm (1/phi^2), pm phi, pm phi, pm phi]
    , [pm (1/phi), pm (1/phi), pm (1/phi), pm (phi^2)]
    ]
  , [ [[0], pm (1/phi^2), pm 1, pm (phi^2)]
    , [[0], pm (1/phi), pm phi, pm (sqrt 5)]
    , [pm (1/phi), pm 1, pm phi, pm 2]
    ]
    >>= choises >>= evenPermutations
  ]
  where
    pm = plusminus

choises :: [[a]] -> [[a]]
choises = sequence

plusminus :: (Num a) => a -> [a]
plusminus a = [a, -a]

-- golden ratio
phi :: (Floating a) => a
phi = (1 + sqrt 5)/2
