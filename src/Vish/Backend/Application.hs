module Vish.Backend.Application where


import qualified Graphics.Rendering.OpenGL.GL           as GL
import Graphics.Rendering.OpenGL                        (($=), get)
import qualified Graphics.UI.GLUT as GLUT

import Data.Monoid

import Vish.Util
import Vish.Graphics.Data.Picture
import Vish.Graphics.Picture
import Vish.Graphics.Data.Texture
import Vish.Graphics.Texture
import Vish.Graphics.Util

import System.Mem

testPicPath = "data/pics/test.jpg"
dicePicPath = "data/pics/dice.png"

testPic :: Picture
testPic =
  image testPicPath
    # translateXY 50 50

dicePic :: Picture
dicePic =
  image dicePicPath
    # translateXY (-50) (-50)

finalPic :: Picture
finalPic =
  testPic <> dicePic

play :: IO ()
play = do
  (_progname, _args) <- GLUT.getArgsAndInitialize
  GLUT.initialDisplayMode $= [ GLUT.DoubleBuffered]
  GLUT.initialWindowSize $=
    GL.Size (fromIntegral 640) (fromIntegral 480)

  _window <- GLUT.createWindow _progname
  GL.depthFunc    $= Just GL.Always

  texCache <- mkTexCache
  installTexture texCache testPicPath
  installTexture texCache dicePicPath

  putStrLn . show $ finalPic

  GLUT.displayCallback $= display texCache
  GLUT.mainLoop

display :: TexCache -> GLUT.DisplayCallback
display texCache =
  withModelview (640, 480) $ do
    GLUT.clear [ GLUT.ColorBuffer ]
    GLUT.blend $= GLUT.Enabled
    GLUT.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)

    drawPicture texCache finalPic

    GLUT.swapBuffers
    performGC
