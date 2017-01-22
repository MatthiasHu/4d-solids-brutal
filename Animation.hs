module Animation where

import Vectors
import Solids
import Construction


animation :: (Floating a, Eq a) => a -> Vec4 a -> Vec4 a
animation t =
    movingInW 1.0 t
  .  crazyRotation t
--  . tiltZ
--  . tiltY
--  . tiltX

object :: (Floating a, Eq a) => Solid (Vec4 a)
object = cell120


tau :: (Floating a) => a
tau = 2*pi

crazyRotation :: (Floating a) => a -> Vec4 a -> Vec4 a
crazyRotation a = 
  rot4dyw (a*2) . rot4dxy (a*0.3) . rot4dzw (a*1.2)

movingInW :: (Floating a) => a -> a -> Vec4 a -> Vec4 a
movingInW amplitude t =
  plus4d $ Vec4 0 0 0 (amplitude * sin (t*0.362))

tiltX :: (Floating a) => Vec4 a -> Vec4 a
tiltX = rot4dxw (atan (1/ sqrt 1))

tiltY :: (Floating a) => Vec4 a -> Vec4 a
tiltY = rot4dyw (atan (1/ sqrt 2))

tiltZ :: (Floating a) => Vec4 a -> Vec4 a
tiltZ = rot4dzw (atan (1/ sqrt 3))
