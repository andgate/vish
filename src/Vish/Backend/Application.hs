module Vish.Backend.Application where

import Graphics.UI.GLUT
import Codec.Picture

play :: IO ()
play = do
  (_progname, _args) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  _window <- createWindow _progname

  displayCallback $= display
  mainLoop

display :: DisplayCallback
display = do
  clear [ ColorBuffer ]
  drawRect (-0.5) (-0.5) 1.0 1.0
  flush
  swapBuffers

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
