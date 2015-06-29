module Vish.Backend.Application where


import qualified Graphics.Rendering.OpenGL.GL           as GL
import Graphics.Rendering.OpenGL                        (($=), get)
import Graphics.UI.GLUT

import Vish.Renderer.Data.Picture
import Vish.Renderer.Picture
import Vish.Renderer.Data.Texture
import Vish.Renderer.Texture
import System.Mem

play :: IO ()
play = do
  (_progname, _args) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  _window <- createWindow _progname
  GL.depthFunc    $= Just GL.Always

  texCache <- initTexCache

  displayCallback $= display texCache
  mainLoop

display :: TexCache -> DisplayCallback
display texCache = do
  clear [ ColorBuffer ]

  drawPicture texCache testPic

  flush
  swapBuffers
  performGC

testPic :: Picture
testPic = Image (Vector2f 100 100) "data/pics/test.jpg"

drawRect :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
drawRect x y h w = do
  let color3f r g b = color $ Color3 r g (b :: GLfloat)
      vertex3f x y z = vertex $ Vertex3 x y (z :: GLfloat)
  renderPrimitive Quads $ do
    color3f 1 0 0
    vertex3f x y 0
    vertex3f x (y+h) 0
    vertex3f (x+w) (y+h) 0
    vertex3f (x+w) y 0
