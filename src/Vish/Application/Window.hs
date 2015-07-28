module Vish.Application.Window where

import Vish.Application.Data.Window
import Vish.Application.Backend.Types

import Control.Monad
import Data.IORef

createWindow :: Backend b => b -> Window -> [Callback] -> IO ()
createWindow backend window callbacks = do
  let debug = False

  backendStateRef <- newIORef backend

  when debug $ putStrLn "* displayInWindow"

  initializeBackend backendStateRef debug

  when debug $ putStrLn "* c window\n"

  openWindow backendStateRef window

  installDisplayCallback backendStateRef callbacks
  
