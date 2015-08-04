module Vish.Application.Window
  ( module Vish.Application.Window
  , module Vish.Application.Data.Window
  )
where

import Vish.Application.Data.App
import Vish.Application.Data.Window
import Vish.Application.Backend.Types
import Vish.Application.Graphics

import Control.Monad
import Data.IORef
import Vish.Application.Data.IORef.Lens

import Graphics.Rendering.OpenGL                        (($=))
import qualified Graphics.Rendering.OpenGL.GL           as GL

createWindow :: (Backend b, AppListener w) => AppRef w -> b -> Callbacks -> IO ()
createWindow appRef backendRef callbacks = do
  let debug = True

  backendRef <- newIORef backendRef
  when debug . putStrLn $ "* displayInWindow"

  initializeBackend backendRef debug
  when debug . putStrLn $ "* c window\n"

  openWindow backendRef =<< appRef ^@ appWindow
  dumpBackendState backendRef

  installCallbacks backendRef callbacks

  -- Dump some debugging info
  when debug $ do
    dumpBackendState backendRef
    -- Not implemented
    --dumpFramebufferState
    --dumpFragmentState

  when debug . putStrLn $ "* entering mainloop.."

  initGraphics
  
  appCreate appRef
  runMainLoop backendRef

  when debug . putStrLn $ "* all done"
