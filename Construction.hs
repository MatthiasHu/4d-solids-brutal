module Construction where

import Solids
import Vectors


point :: Solid [a]
point = Solid [[]]

prism :: a -> a -> Solid [a] -> Solid [a]
prism a b (Solid ps) = Solid $ map (a:) ps ++ map (b:) ps

prism' :: (Num a) => Solid [a] -> Solid [a]
prism' = prism (-1) 1


cube :: (Num a) => Solid (Vec3 a)
cube = fmap fromList3d $ prism' . prism' . prism' $ point
hypercube :: (Num a) => Solid (Vec4 a)
hypercube = fmap fromList4d $ prism' . prism' . prism' . prism' $ point
