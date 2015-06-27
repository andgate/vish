module Vish.Backend.Application where

import Graphics.UI.GLUT

play :: IO ()
play = do
  (_progname, _args) <- getArgsAndInitialize
  _window <- createWindow "Hello world"
  displayCallback $= display
  mainLoop

display :: DisplayCallback
display = do
  clear [ ColorBuffer ]
  flush
