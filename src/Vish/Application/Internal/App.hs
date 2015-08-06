module Vish.Application.Internal.App where

import           Vish.Application.Data.App
import           Vish.Application.Data.Input
import           Vish.Application.Data.Window
import           Vish.Application.Internal.Backend
import           Vish.Application.AppConfig
import           Vish.Application.Internal.Input
import           Vish.Application.Internal.Window
import           Vish.Graphics.Picture
import           Vish.Application.Graphics

import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Data.IORef
import           Vish.Application.Data.IORef.Lens
import           Data.Yaml
import           Data.Maybe (fromMaybe)
import qualified System.Mem  as System

playWithBackend :: (Backend b, AppListener w) => b -> w -> IO ()
playWithBackend backend world = do
  backendRef <- newIORef backend
  appRef <- newIORef =<< mkApp world

  appConfig <- loadAppConfig

  let win = appConfigToWindow appConfig
  appRef & appWindow @~ win

  let callbacks =
        Callbacks
        { displayCallback     = displayUpdate appRef
        , pauseCallback       = pauseApplication appRef
        , resumeCallback      = resumeApplication appRef
        , closeCallback       = disposeApplication appRef
        , reshapeCallback     = resizeWindow appRef
        , keyboardCallback    = updateKeyboardInput appRef
        , mouseMoveCallback   = updateMouseMoveInput appRef
        , mouseButtonCallback = updateMouseClickInput appRef
        , scrollCallback      = updateScrolledInput appRef
        }

  createWindow appRef backendRef callbacks

displayUpdate :: (AppListener w, Backend b) => AppRef w -> IORef b -> IO ()
displayUpdate appRef backendRef = do
  updateInput appRef backendRef
  appUpdate appRef

  pic <- appDraw appRef
  texCache <- appRef ^@ appGfx.gfxTexCache
  winSize <- appRef ^@ appWindow.windowSize

  renderStart
  displayPicture texCache pic winSize
  renderEnd

  appPostUpdate appRef

  handleAppStatus appRef backendRef

  System.performGC

handleAppStatus :: (AppListener w, Backend b) => AppRef w -> IORef b -> IO ()
handleAppStatus appRef backendRef = do
  shouldQuit <- appRef^@appStatus
  case shouldQuit of
    AppPlay -> return ()
    AppQuit -> exitBackend backendRef

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
