module Vish.Application.Window where

import Vish.Application.Data.Window
import Vish.Application.Backend.Types

import Control.Monad
import Data.IORef

import Graphics.Rendering.OpenGL                        (($=))
import qualified Graphics.Rendering.OpenGL.GL           as GL

createWindow :: Backend b => b -> Window -> Callbacks -> IO ()
createWindow backend window callbacks = do
  let debug = False

  backendStateRef <- newIORef backend
  when debug . putStrLn $ "* displayInWindow"

  initializeBackend backendStateRef debug
  when debug . putStrLn $ "* c window\n"

  openWindow backendStateRef window

  installDisplayCallback     backendStateRef callbacks
  installWindowCloseCallback backendStateRef
  installReshapeCallback     backendStateRef callbacks
  installKeyMouseCallback    backendStateRef callbacks
  installMotionCallback      backendStateRef callbacks
  installIdleCallback        backendStateRef callbacks

  GL.depthFunc $= Just GL.Always
  --GL.clearColor   $= glColor4OfColor clearColor

  -- Dump some debugging info
  when debug $ do
    dumpBackendState backendStateRef
    -- Not implemented
    --dumpFramebufferState
    --dumpFragmentState

  when debug . putStrLn $ "* entering mainloop.."

  runMainLoop backendStateRef

  when debug . putStrLn $ "* all done"
