
import Graphics.UI.GLUT
import Data.IORef
import Control.Concurrent (threadDelay)
import Control.Monad

import Solids
import Vectors
import VectorsToGL
import Render
import Construction


data State = State
  { angle :: GLfloat
  }

initialState = State
  { angle = 0
  }


main = do
  getArgsAndInitialize
  createWindow "4d solids"
  glSetup
  stateRef <- newIORef initialState
  displayCallback       $= (get stateRef >>= display)
  idleCallback          $= Just (idle stateRef)
  mainLoop


glSetup :: IO ()
glSetup = do
  depthFunc $= Just Lequal
  matrixMode $= Projection
  loadIdentity
  let near = 0.01
  frustum (-0.5*near) (0.5*near) (-0.5*near) (0.5*near) near 1000
  matrixMode $= Modelview 0
  loadIdentity
  translate $ (Vector3 0 0 (-5.0) :: Vector3 GLfloat)
  -- lighting
  lighting $= Enabled
  lightModelLocalViewer $= Enabled
  materialEmission FrontAndBack $= Color4 0.0 0.0 0.0 1.0
  materialSpecular FrontAndBack $= Color4 1.0 1.0 1.0 1.0
  colorMaterial $= Just (FrontAndBack, AmbientAndDiffuse)
  light (Light 0) $= Enabled
  ambient (Light 0) $= Color4 0.1 0.1 0.1 1
  diffuse (Light 0) $= Color4 1.0 1.0 1.0 1
  specular (Light 0) $= Color4 0.2 0.2 0.2 1
  attenuation (Light 0) $= (0 , 0.3, 0.05)

display :: State -> IO ()
display state = do
  clearColor $= Color4 0 0.2  0 0
  clear [ColorBuffer, DepthBuffer]
  color' 0 0 1
  rotate' 1.0 (Vector3 1 2 0)
  renderSolid cube
  flush

color' :: GLfloat -> GLfloat -> GLfloat -> IO ()
color' r g b = color $ Color4 r g b 1

vertex' :: GLfloat -> GLfloat -> GLfloat -> IO ()
vertex' x y z = vertex $ Vertex3 x y z

scale' :: GLfloat -> IO ()
scale' s = scale s s s

rotate' :: GLfloat -> Vector3 GLfloat -> IO ()
rotate' = rotate

--renderSolid :: Solid (Vec3 GLfloat) -> IO ()
renderSolid solid = renderPrimitive Triangles $
  mapM renderTriangle $ allTriangles solid
  where
    renderTriangle :: (Vec3 GLfloat, Vec3 GLfloat, Vec3 GLfloat) -> IO ()
    renderTriangle (a, b, c) = do
      normal (toGLNormal $ planeNormal a b c)
      vertex (toGLVertex3 a)
      vertex (toGLVertex3 b)
      vertex (toGLVertex3 c)

idle :: IORef State -> IO ()
idle ref = do
  st <- get ref
  ref $= step st
  threadDelay $ 10^4
  putStrLn "Yo"
  postRedisplay Nothing

step :: State -> State
step s = State (angle s + 0.3)
