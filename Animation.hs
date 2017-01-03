module Animation where

import Vectors
import Solids
import Construction


animation :: (Floating a) => a -> Solid (Vec4 a)
animation t =
    movingInW 1.0 t
  . crazyRotation t
--  . tiltZ
--  . tiltY
--  . tiltX
  $ cell16


tau :: (Floating a) => a
tau = 2*pi

crazyRotation :: (Floating a) => a -> Solid (Vec4 a) -> Solid (Vec4 a)
crazyRotation a = 
  fmap (rot4dyw (a*2) . rot4dxy (a*0.3) . rot4dzw (a*1.2))

movingInW :: (Floating a) => a -> a -> Solid (Vec4 a) -> Solid (Vec4 a)
movingInW amplitude t =
  fmap $ plus4d $ Vec4 0 0 0 (amplitude * sin (t*0.362))

tiltX :: (Floating a) => Solid (Vec4 a) -> Solid (Vec4 a)
tiltX = fmap $ rot4dxw (atan (1/ sqrt 1))

tiltY :: (Floating a) => Solid (Vec4 a) -> Solid (Vec4 a)
tiltY = fmap $ rot4dyw (atan (1/ sqrt 2))

tiltZ :: (Floating a) => Solid (Vec4 a) -> Solid (Vec4 a)
tiltZ = fmap $ rot4dzw (atan (1/ sqrt 3))
