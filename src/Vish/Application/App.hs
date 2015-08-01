module Vish.Application.App where

import           Vish.Application.Data.App
import           Vish.Application.Data.Input
import           Vish.Application.Data.Window
import           Vish.Application.Input
import           Vish.Application.Window
import           Vish.Application.Backend
import           Vish.Graphics.Picture

import           Control.Lens
import           Data.IORef

import           Graphics.Rendering.OpenGL    (get, ($=))
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.UI.GLUT             as GLUT


play :: AppListener w => w -> IO ()
play = playWithBackend defaultBackendState

playWithBackend :: (Backend b, AppListener w) => b -> w -> IO ()
playWithBackend backend world = do
  app <- appStart =<< mkApp world
  appRef <- newIORef app

  let callbacks = defaultCallbacks
        { displayCallback     = displayUpdate appRef
        , keyboardCallback    = updateKeyboardInput appRef
        , mouseMoveCallback   = updateMouseMoveInput appRef
        , mouseButtonCallback = updateMouseClickInput appRef
        , scrollCallback      = updateScrolledInput appRef
        }
      window = Window
        { _winName = "Blank",
          _winX = 0,
          _winY = 0,
          _winW = 640,
          _winH = 480,
          _winState = Windowed
        }

  registerInputListener appRef InputPrinter

  createWindow backend window callbacks

displayUpdate :: (AppListener w, Backend b) => IORef (App w) -> IORef b -> GLUT.DisplayCallback
displayUpdate appRef backendStateRef = do
  app <- readIORef appRef
  (pic, app') <- appDraw =<< appUpdate app

  let texCache = app'^.appGfx.gfxTexCache
  displayPicture texCache pic

  appPostUpdate app' >>= writeIORef appRef

data InputPrinter = InputPrinter

instance InputListener InputPrinter where
  keyPressed _ key = print key
  mouseButtonClicked _ button = print button
  mouseMoved _ x y = putStrLn $ "Mouse Moved: " ++ show (x,y)
  scrolled _ x y = putStrLn $ "Scrolled: " ++ show (x,y)

registerInputListener :: InputListener l => IORef (App w) -> l -> IO ()
registerInputListener appRef listener = do
  let reg = register listener
  modifyIORef appRef $ appInput.inputListeners %~ (reg:)
