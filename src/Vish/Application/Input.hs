module Vish.Application.Input where

import Vish.Application.Data.App
import Vish.Application.Data.Input
import Vish.Application.Backend

import Control.Lens
import Control.Monad
import qualified Data.HashTable.IO as H
import Data.IORef

updateKeyTable :: KeyTable -> Key -> KeyState -> IO ()
updateKeyTable =
  H.insert

updateButtonTable :: ButtonTable -> MouseButton -> KeyState -> IO ()
updateButtonTable =
  H.insert

updateKeyboardInput :: Backend a => AppRef w -> IORef a -> Key -> KeyState -> Modifiers -> IO ()
updateKeyboardInput appRef _ key keystate mods = do
  input <- liftM (^.appInput) (readIORef appRef)
  updateKeyTable (input^.inputKeyTable) key keystate
  case keystate of
    Down ->
      mapM_ (\(MkInputListener a) -> keyPressed a key) (input^.inputListeners)
    Up ->
      mapM_ (\(MkInputListener a) -> keyReleased a key) (input^.inputListeners)
    Held ->
      mapM_ (\(MkInputListener a) -> keyHeld a key) (input^.inputListeners)

updateMouseMoveInput :: Backend a => AppRef w -> IORef a
                        -> Double -> Double -> IO ()
updateMouseMoveInput appRef _ moveX moveY = do
  listeners <- liftM (^.appInput.inputListeners) (readIORef appRef)
  mapM_ (\(MkInputListener a) -> mouseMoved a moveX moveY) listeners

updateMouseClickInput :: Backend a => AppRef w -> IORef a
                        -> MouseButton -> KeyState -> Modifiers -> IO ()
updateMouseClickInput appRef _ button keystate mods = do
  input <- liftM (^.appInput) (readIORef appRef)
  updateButtonTable (input^.inputButtonTable) button keystate
  case keystate of
    Down ->
      mapM_ (\(MkInputListener a) -> mouseButtonClicked a button) (input^.inputListeners)
    Up ->
      mapM_ (\(MkInputListener a) -> mouseButtonReleased a button) (input^.inputListeners)
    Held ->
      mapM_ (\(MkInputListener a) -> mouseButtonHeld a button) (input^.inputListeners)

updateScrolledInput :: Backend a => AppRef w -> IORef a
                        -> Double -> Double -> IO ()
updateScrolledInput appRef _ scrollX scrollY = do
  listeners <- liftM (^.appInput.inputListeners) (readIORef appRef)
  mapM_ (\(MkInputListener a) -> scrolled a scrollX scrollY) listeners
