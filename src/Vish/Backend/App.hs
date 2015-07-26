module Vish.Backend.App where

import           Vish.Data.App

import           Graphics.Rendering.OpenGL    (get, ($=))
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.UI.GLUT             as GLUT

import           Control.Lens

import           Vish.Graphics.Picture
import           Vish.Graphics.Util

import           Data.IORef
import           System.Mem

play :: AppListener w => w -> IO ()
play world = do
  (_progname, _args) <- GLUT.getArgsAndInitialize
  GLUT.initialDisplayMode $= [ GLUT.DoubleBuffered]
  GLUT.initialWindowSize $=
    GL.Size (fromIntegral 640) (fromIntegral 480)

  _window <- GLUT.createWindow _progname
  GL.depthFunc    $= Just GL.Always

  app <- appStart =<< mkApp world
  appRef <- newIORef app

  GLUT.displayCallback $= displayUpdate appRef
  GLUT.mainLoop

displayUpdate :: AppListener w => IORef (App w) -> GLUT.DisplayCallback
displayUpdate appRef =
  withModelview (640, 480) $ do
    GLUT.clear [ GLUT.ColorBuffer ]
    GLUT.blend $= GLUT.Enabled
    GLUT.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)

    app <- readIORef appRef
    (pic, app') <- appDraw =<< appUpdate app

    let texCache = app'^.appGfx.gfxTexCache
    drawPicture texCache pic

    appPostUpdate app' >>= writeIORef appRef

    GLUT.swapBuffers
    performGC
