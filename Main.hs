
import Graphics.UI.GLUT
import Data.IORef
import Control.Concurrent (threadDelay)
import qualified Data.ByteString as BS
import Debug.Trace

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
  , solids :: [[(Vec3 GLfloat, Vec3 GLfloat, Vec3 GLfloat)]]
  }

initialState :: State
initialState = State
  { time = 0.001
  , angleXZ = tau/9
  , angleYZ = tau/7
  , solids = boundingTrianglesSequence
    . intersectXYZ
    . fmap (rot4dyw (tau*2/17) . rot4dxw (tau/9))
    $ hypercube
  }

data ShaderLocations = ShaderLocations
  { aNormal :: AttribLocation
  }


main :: IO ()
main = do
  shaderLocations <- glSetup
  stateRef <- newIORef initialState
  displayCallback       $= (get stateRef >>= display shaderLocations)
  idleCallback          $= Just (idle stateRef)
  keyboardCallback      $= Just (keyboardInput stateRef)
  mainLoop


glSetup :: IO ShaderLocations
glSetup = do
  _ <- getArgsAndInitialize
  initialWindowSize $= Size 500 500
  _ <- createWindow "4d solids"
  windowPosition $= Position 0 0
  depthFunc $= Just Lequal
  vertShaderSource <- BS.readFile "vertshader.sl"
  fragShaderSource <- BS.readFile "fragshader.sl"
  attLocNormal <-
    setupShaderProgram vertShaderSource fragShaderSource
  return $ ShaderLocations attLocNormal

setupShaderProgram ::
  BS.ByteString -> BS.ByteString -> IO AttribLocation
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
  infoLog <- programInfoLog prog
  putStrLn $ infoLog
  currentProgram $= Just prog
  attLocNormal <- get $ attribLocation prog "aNormal"
  return attLocNormal

keyboardInput :: IORef State -> KeyboardCallback
keyboardInput ref c _ = modifyState ref $ keyboard c

keyboard :: Char -> State -> State
keyboard ' ' s  = s {solids = tail (solids s)}
keyboard 'h' s  = s {angleXZ = angleXZ s +0.1}
keyboard 'l' s  = s {angleXZ = angleXZ s -0.1}
keyboard _ s    = s

modifyState :: IORef State -> (State -> State) -> IO ()
modifyState ref f = modifyIORef ref f >> postRedisplay Nothing

display :: ShaderLocations -> State -> IO ()
display shaderLocs s = do
  clearColor $= Color4 0 0.2 0.2 0
  clear [ColorBuffer, DepthBuffer]
--  renderTriangles shaderLocs
--    . map (mapTriangle $ rot3dyz (angleYZ s) . rot3dxz (angleXZ s))
--    $ head (solids s)
  renderSolid shaderLocs
    . fmap (rot3dyz (angleYZ s) . rot3dxz (angleXZ s))
    . intersectXYZRegular
    $ animation (time s)
  flush

mapTriangle :: (a -> b) -> (a, a, a) -> (b, b, b)
mapTriangle f (x, y, z) = (f x, f y, f z)

vertexAttrib3' :: AttribLocation -> Vec3 GLfloat -> IO ()
vertexAttrib3' loc (Vec3 x y z) = vertexAttrib3 loc x y z

renderSolid :: ShaderLocations -> Solid (Vec3 GLfloat) -> IO ()
renderSolid shaderLocs =
  renderTriangles shaderLocs . boundingTriangles

renderTriangles :: ShaderLocations ->
  [(Vec3 GLfloat, Vec3 GLfloat, Vec3 GLfloat)] -> IO ()
renderTriangles shaderLocs tris =
  (trace $ "rendering " ++ show (length tris) ++ " triangles")
  . renderPrimitive Triangles . mapM_ renderTriangle $ tris
  where
    renderTriangle :: (Vec3 GLfloat, Vec3 GLfloat, Vec3 GLfloat) -> IO ()
    renderTriangle (a, b, c) = do
      vertexAttrib3' (aNormal shaderLocs) $ planeNormal a b c
      vertex (toGLVertex3 a)
      vertex (toGLVertex3 b)
      vertex (toGLVertex3 c)

idle :: IORef State -> IO ()
idle ref = do
  threadDelay 30000
  modifyState ref step

step :: State -> State
step s = s {time = time s + 0.02}
