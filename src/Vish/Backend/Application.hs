{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Vish.Backend.Application where


import           Graphics.Rendering.OpenGL    (get, ($=))
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.UI.GLUT             as GLUT

import           Control.Lens

import           Data.Monoid

import           Vish.Graphics.Data.Picture
import           Vish.Graphics.Data.Texture
import           Vish.Graphics.Picture
import           Vish.Graphics.Texture
import           Vish.Graphics.Util
import           Vish.Util

import           Data.IORef
import           System.Mem

class World w where
  worldUpdate :: w -> IO w
  worldDraw :: w -> IO (Picture, w)
  worldPostUpdate :: w -> IO w

data App w = App
  { _appTexCache :: TexCache,
    _appWorld :: w
  }

makeLenses ''App

mkApp :: World w => w -> IO (App w)
mkApp w = do
  texCache <- mkTexCache
  return $ App texCache w

play :: World w => w -> IO ()
play world = do
  (_progname, _args) <- GLUT.getArgsAndInitialize
  GLUT.initialDisplayMode $= [ GLUT.DoubleBuffered]
  GLUT.initialWindowSize $=
    GL.Size (fromIntegral 640) (fromIntegral 480)

  _window <- GLUT.createWindow _progname
  GL.depthFunc    $= Just GL.Always

  app <- mkApp world
  appRef <- newIORef app

  GLUT.displayCallback $= displayUpdate appRef
  GLUT.mainLoop

displayUpdate :: World w => IORef (App w) -> GLUT.DisplayCallback
displayUpdate appRef =
  withModelview (640, 480) $ do
    GLUT.clear [ GLUT.ColorBuffer ]
    GLUT.blend $= GLUT.Enabled
    GLUT.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)

    app <- readIORef appRef
    (pic, world') <- worldDraw =<< worldUpdate (app^.appWorld)
    writeIORef appRef $ (appWorld .~ world') app

    drawPicture (app^.appTexCache) pic

    GLUT.swapBuffers
    performGC
