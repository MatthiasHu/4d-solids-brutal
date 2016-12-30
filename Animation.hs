module Animation where

import Vectors
import Solids
import Construction


animation :: (Floating a) => a -> Solid (Vec4 a)
animation a =
--    movingInW a
--    crazyRotation a
--    fmap (rot4dxy a)
  hypercube


tau :: (Floating a) => a
tau = 2*pi

crazyRotation :: (Floating a) => a -> Solid (Vec4 a) -> Solid (Vec4 a)
crazyRotation a = 
  fmap (rot4dyw (a*3) . rot4dxy (a*0.2) . rot4dzw (a*2.2))

movingInW :: (Floating a) => a -> Solid (Vec4 a) -> Solid (Vec4 a)
movingInW a = 
  fmap $ plus4d $ Vec4 0 0 0 (sqrt 4 * cos (a*0.76842))

tiltX :: (Floating a) => Solid (Vec4 a) -> Solid (Vec4 a)
tiltX = fmap $ rot4dxw (atan (1/ sqrt 1))

tiltY :: (Floating a) => Solid (Vec4 a) -> Solid (Vec4 a)
tiltY = fmap $ rot4dyw (atan (1/ sqrt 2))

tiltZ :: (Floating a) => Solid (Vec4 a) -> Solid (Vec4 a)
tiltZ = fmap $ rot4dzw (atan (1/ sqrt 3))
