
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
import Construction


data State = State
  { time :: GLfloat
  , angleXZ :: GLfloat
  , angleYZ :: GLfloat
  }

initialState = State
  { time = 0
  , angleXZ = tau/9
  , angleYZ = tau/7
  }

data ShaderLocations = ShaderLocations
  { aNormal :: AttribLocation
  }


main = do
  shaderLocations <- glSetup
  stateRef <- newIORef initialState
  displayCallback       $= (get stateRef >>= display shaderLocations)
  idleCallback          $= Just (idle stateRef)
  keyboardCallback      $= Just (keyboardInput stateRef)
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

keyboardInput :: IORef State -> KeyboardCallback
keyboardInput ref c _ = modifyState ref $ keyboard c

keyboard :: Char -> State -> State
keyboard ' ' s  = s {angleXZ = angleXZ s + 0.5}
keyboard _ s    = s

modifyState :: IORef State -> (State -> State) -> IO ()
modifyState ref f = modifyIORef ref f >> postRedisplay Nothing

display :: ShaderLocations -> State -> IO ()
display shaderLocs s = do
  clearColor $= Color4 0 0.2  0 0
  clear [ColorBuffer, DepthBuffer]
  renderSolid shaderLocs
    . fmap (rot3dyz (angleYZ s) . rot3dxz (angleXZ s))
    $ cube
--    . intersectXYZ
--    $ animation (angle state)
  flush

vertexAttrib3' :: AttribLocation -> Vec3 GLfloat -> IO ()
vertexAttrib3' loc (Vec3 x y z) = vertexAttrib3 loc x y z

renderSolid :: ShaderLocations -> Solid (Vec3 GLfloat) -> IO ()
renderSolid shaderLocs solid = renderPrimitive Triangles $
  mapM_ renderTriangle $ boundingTriangles solid
  where
    renderTriangle :: (Vec3 GLfloat, Vec3 GLfloat, Vec3 GLfloat) -> IO ()
    renderTriangle (a, b, c) = do
      vertexAttrib3' (aNormal shaderLocs) $ planeNormal a b c
      vertex (toGLVertex3 a)
      vertex (toGLVertex3 b)
      vertex (toGLVertex3 c)

idle :: IORef State -> IO ()
idle ref = do
  threadDelay 1000000
  modifyState ref step

step :: State -> State
step s = s {time = time s + 0.05}
