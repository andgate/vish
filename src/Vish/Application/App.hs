module Vish.Application.App where

import           Vish.Application.Data.App
import           Vish.Application.Data.Window
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
        { displayCallback = displayUpdate appRef
        , keyboardCallback = printKey
        , mouseButtonCallback = printMouseButton
        , scrollCallback = printScroll
        }
      window = Window
        { _winName = "Blank",
          _winX = 0,
          _winY = 0,
          _winW = 640,
          _winH = 480,
          _winState = Windowed
        }

  createWindow backend window callbacks

displayUpdate :: (AppListener w, Backend b) => IORef (App w) -> IORef b -> GLUT.DisplayCallback
displayUpdate appRef backendStateRef = do
  app <- readIORef appRef
  (pic, app') <- appDraw =<< appUpdate app

  let texCache = app'^.appGfx.gfxTexCache
  displayPicture texCache pic

  appPostUpdate app' >>= writeIORef appRef

printKey :: Backend a => IORef a -> Key -> KeyState -> Modifiers -> IO ()
printKey _ key keystate _ =
  putStrLn $ show key ++ ": " ++ show keystate

printMouseButton :: Backend a => IORef a -> MouseButton -> KeyState -> Modifiers -> IO ()
printMouseButton _ button keystate _ =
  putStrLn $ show button ++ ": " ++ show keystate

printScroll :: Backend a => IORef a -> Double -> Double -> IO ()
printScroll _ x y =
  putStrLn $ "Scroll: " ++ show (x,y)
