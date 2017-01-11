module Construction where

import Solids
import Vectors


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


cube :: (Num a) => Solid (Vec3 a)
cube = fmap fromList3d $ prism' . prism' . prism' $ point

hypercube, tesseract, cell8 :: (Num a) => Solid (Vec4 a)
hypercube = fmap fromList4d $ prism' . prism' . prism' . prism' $ point
tesseract = hypercube
cell8 = hypercube

tetraeder :: (Num a) => Solid (Vec3 a)
tetraeder = fmap fromList3d $ crossPolytope 3

cell16 :: (Num a) => Solid (Vec4 a)
cell16 = fmap fromList4d $ crossPolytope 4

hyperdiamond :: (Floating a) => Solid (Vec4 a)
hyperdiamond = Solid . map normalize4d $ as ++ bs
  where
    Solid as = hypercube
    Solid bs = cell16
