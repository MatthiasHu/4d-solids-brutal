module Intersect where

import Solids
import Vectors


-- abstract intersection with a (hyper-) plane
intersect :: (p -> Bool) -> Solid p -> Solid (p, p)
intersect t (Solid ps) = Solid $ do
  x <- filter t ps
  y <- filter (not . t) ps
  return (x, y)


-- intersect 4d solid with x-y-z-hyperplane to get 3d solid in 3d space
intersectXYZ :: (Fractional s, Ord s) => Solid (Vec4 s) -> Solid (Vec3 s)
intersectXYZ = fmap interpolate . intersect ((<0) . coord4dw)
  where
    interpolate :: (Fractional s) => (Vec4 s, Vec4 s) -> Vec3 s
    interpolate (a, b) = plus3d
      ( (coord4dw b / dw) `smult3d` coord4dxyz a )
      ((-coord4dw a / dw) `smult3d` coord4dxyz b )
      where dw = coord4dw b - coord4dw a
