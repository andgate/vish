module Vish.Backend.Application where


import qualified Graphics.Rendering.OpenGL.GL           as GL
import Graphics.Rendering.OpenGL                        (($=), get)
import qualified Graphics.UI.GLUT as GLUT

import Vish.Renderer.Data.Picture
import Vish.Renderer.Picture
import Vish.Renderer.Data.Texture
import Vish.Renderer.Texture
import Vish.Renderer.Util

import System.Mem

play :: IO ()
play = do
  (_progname, _args) <- GLUT.getArgsAndInitialize
  GLUT.initialDisplayMode $= [ GLUT.RGBMode, GLUT.DoubleBuffered]
  GLUT.initialWindowSize $=
    GL.Size (fromIntegral 640) (fromIntegral 480)

  _window <- GLUT.createWindow _progname
  GL.depthFunc    $= Just GL.Always

  texCache <- initTexCache

  GLUT.displayCallback $= display texCache
  GLUT.mainLoop

display :: TexCache -> GLUT.DisplayCallback
display texCache =
  withModelview (640, 480) $ do
    GLUT.clear [ GLUT.ColorBuffer ]

    drawPicture texCache testPic

    GLUT.flush
    GLUT.swapBuffers
    performGC

testPic :: Picture
testPic = Image (Vector2f 100 100) "data/pics/test.jpg"
