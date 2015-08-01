module Vish.Application.Input where

import Vish.Application.Data.App
import Vish.Application.Data.Input
import Vish.Application.Backend

import Control.Lens
import Control.Monad
import qualified Data.HashTable.IO as H
import Data.IORef

updateKeyboardInput :: Backend a => IORef (App w) -> IORef a -> Key -> KeyState -> Modifiers -> IO ()
updateKeyboardInput appRef _ key keystate mods = do
  input <- liftM (^.appInput) (readIORef appRef)
  updateKeyTable (input^.inputKeyTable) key keystate
  case keystate of
    Down ->
      mapM_ (\(MkInputListener a) -> keyDown a key) (input^.inputListeners)
    Up ->
      mapM_ (\(MkInputListener a) -> keyUp a key) (input^.inputListeners)
    Held ->
      mapM_ (\(MkInputListener a) -> keyDown a key) (input^.inputListeners)

updateKeyTable :: KeyTable -> Key -> KeyState -> IO ()
updateKeyTable =
  H.insert
