module VectorsToGL where

import Graphics.Rendering.OpenGL

import Vectors


toGLVertex3 :: Vec3 a -> Vertex3 a
toGLVertex3 (Vec3 x y z) = Vertex3 x y z

toGLNormal :: Vec3 a -> Normal3 a
toGLNormal (Vec3 x y z) = Normal3 x y z
