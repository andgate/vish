module Vish.Application.App where

import           Vish.Application.Data.App
import           Vish.Application.Data.Input
import           Vish.Application.Data.Window
import           Vish.Application.Input
import           Vish.Application.Window
import           Vish.Application.Backend
import           Vish.Graphics.Picture

import           Control.Lens
import           Control.Monad
import           Data.IORef
import           Data.Yaml
import           Data.Maybe (fromMaybe)

import           Graphics.Rendering.OpenGL    (get, ($=))
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.UI.GLUT             as GLUT

play :: AppListener w => w -> IO ()
play = playWithBackend defaultBackendState

playWithBackend :: (Backend b, AppListener w) => b -> w -> IO ()
playWithBackend backend world = do
  appRef <- newIORef =<< mkApp world
  appCreate appRef

  window <- loadWindow

  let callbacks =
        Callbacks
        { displayCallback     = displayUpdate appRef
        , appPauseCallback    = pauseApplication appRef
        , appResumeCallback   = resumeApplication appRef
        , closeCallback       = disposeApplication appRef
        , reshapeCallback     = resizeWindow appRef
        , keyboardCallback    = updateKeyboardInput appRef
        , mouseMoveCallback   = updateMouseMoveInput appRef
        , mouseButtonCallback = updateMouseClickInput appRef
        , scrollCallback      = updateScrolledInput appRef
        }

  registerInputListener appRef InputPrinter

  createWindow backend window callbacks

displayUpdate :: (AppListener w, Backend b) => AppRef w -> IORef b -> IO ()
displayUpdate appRef backendStateRef = do
  appUpdate appRef
  pic <- appDraw appRef

  app <- readIORef appRef
  let texCache = app^.appGfx.gfxTexCache
  displayPicture texCache pic

  appPostUpdate appRef



resizeWindow :: (AppListener w, Backend b) => AppRef w -> IORef b -> Int -> Int -> IO ()
resizeWindow app _ =
  appResize app

pauseApplication :: (AppListener w, Backend b) => AppRef w -> IORef b -> IO ()
pauseApplication app _ =
  appPause app

resumeApplication :: (AppListener w, Backend b) => AppRef w -> IORef b -> IO ()
resumeApplication app _ =
  appResume app

disposeApplication :: (AppListener w, Backend b) => AppRef w -> IORef b -> IO ()
disposeApplication app _ =
  appDispose app

data InputPrinter = InputPrinter

instance InputListener InputPrinter where
  keyPressed _ key = print key
  mouseButtonClicked _ button = print button
  mouseMoved _ x y = putStrLn $ "Mouse Moved: " ++ show (x,y)
  scrolled _ x y = putStrLn $ "Scrolled: " ++ show (x,y)

registerInputListener :: InputListener l => AppRef w -> l -> IO ()
registerInputListener appRef listener = do
  let reg = register listener
  modifyIORef appRef $ appInput.inputListeners %~ (reg:)
