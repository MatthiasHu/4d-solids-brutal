
import Graphics.UI.GLUT
import Data.IORef
import Control.Concurrent (threadDelay)
import Control.Monad
import qualified Data.ByteString as BS

import Solids
import Vectors
import VectorsToGL
import Render
import Intersect
import Animation


data State = State
  { angle :: GLfloat
  }

initialState = State
  { angle = 0
  }

data ShaderLocations = ShaderLocations
  { aNormal :: AttribLocation
  }


main = do
  shaderLocations <- glSetup
  stateRef <- newIORef initialState
  displayCallback       $= (get stateRef >>= display shaderLocations)
  idleCallback          $= Just (idle stateRef)
  mainLoop


glSetup :: IO ShaderLocations
glSetup = do
  getArgsAndInitialize
  initialWindowSize $= Size 500 500
  createWindow "4d solids"
  windowPosition $= Position 0 0
  depthFunc $= Just Lequal
  vertShaderSource <- BS.readFile "vertshader.sl"
  fragShaderSource <- BS.readFile "fragshader.sl"
  (prog, attLocNormal) <-
    setupShaderProgram vertShaderSource fragShaderSource
  return $ ShaderLocations attLocNormal

setupShaderProgram ::
  BS.ByteString -> BS.ByteString -> IO (Program, AttribLocation)
setupShaderProgram vertSource fragSource = do
  vertShader <- createShader VertexShader
  fragShader <- createShader FragmentShader
  shaderSourceBS vertShader $= vertSource
  shaderSourceBS fragShader $= fragSource
  compileShader vertShader
  compileShader fragShader
  prog <- createProgram
  attachShader prog vertShader
  attachShader prog fragShader
  linkProgram prog
  log <- programInfoLog prog
  putStrLn $ log
  currentProgram $= Just prog
  attLocNormal <- get $ attribLocation prog "aNormal"
  return (prog, attLocNormal)

display :: ShaderLocations -> State -> IO ()
display shaderLocs state = do
  clearColor $= Color4 0 0.2  0 0
  clear [ColorBuffer, DepthBuffer]
  -- change solid and animation here
  renderSolid shaderLocs
    . fmap (rot3dyz (tau/9) . rot3dxz (tau/7))
    . intersectXYZ
    $ animation (angle state)
  flush

vertexAttrib3' :: AttribLocation -> Vec3 GLfloat -> IO ()
vertexAttrib3' loc (Vec3 x y z) = vertexAttrib3 loc x y z

renderSolid :: ShaderLocations -> Solid (Vec3 GLfloat) -> IO ()
renderSolid shaderLocs solid = renderPrimitive Triangles $
  mapM_ renderTriangle $ allTriangles solid
  where
    renderTriangle :: (Vec3 GLfloat, Vec3 GLfloat, Vec3 GLfloat) -> IO ()
    renderTriangle (a, b, c) = do
      vertexAttrib3' (aNormal shaderLocs) $ planeNormal a b c
      vertex (toGLVertex3 a)
      vertex (toGLVertex3 b)
      vertex (toGLVertex3 c)

idle :: IORef State -> IO ()
idle ref = do
  threadDelay 100000
  st <- get ref
  ref $= step st
  threadDelay $ 10^4
--  putStrLn "Yo"
  postRedisplay Nothing

step :: State -> State
step s = State (angle s + 0.05)
