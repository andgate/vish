module Vish.Application.Internal.Window where

import Vish.Application.Data.App
import Vish.Application.Internal.Backend.Types
import Vish.Application.Graphics

import Control.Monad
import Data.IORef
import Vish.Application.Data.IORef.Lens

createWindow :: (Backend b, AppListener w) => AppRef w -> IORef b -> Callbacks -> IO ()
createWindow appRef backendRef callbacks = do
  let debug = True
  initializeBackend backendRef debug
  openWindow backendRef =<< appRef ^@ appWindow
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
