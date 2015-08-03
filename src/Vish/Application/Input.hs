{-# LANGUAGE ExistentialQuantification #-}
module Vish.Application.Input
  ( module Vish.Application.Input
  , module Vish.Application.Data.Input
  )
where

import Vish.Application.Data.App
import Vish.Application.Data.Input
import qualified Vish.Application.Backend as B

import Control.Arrow
import Control.Lens
import Control.Monad
import Data.IORef
import Data.Hashable
import qualified Data.HashTable.IO as H


-- Input caching --------------------------------------------------------------
-- Stores input in table for monitoring held keys.
-- Removes input on release. This keeps the tables small.
cacheInput :: (Hashable k, Eq k) => InputTable k -> k -> B.InputState -> IO (Maybe InputState)
cacheInput table key state = do
  let state' = B.fromBackend state
  case state of
    B.Down -> do
      maybeOldState <- H.lookup table key
      case maybeOldState of
        Nothing -> do
          H.insert table key state'
          return $ Just state'
        Just _ -> return Nothing
    B.Up -> do
      H.delete table key
      return $ Just state'

cleanInputCache :: AppRef w -> IO ()
cleanInputCache ref = do
  keyTable <- H.new
  buttonTable <- H.new
  modifyIORef ref $ appInput.inputKeyTable .~ keyTable
  modifyIORef ref $ appInput.inputButtonTable .~ buttonTable

-- Input update --------------------------------------------------------------
-- | Updates the input in the tables
updateInput :: B.Backend a => AppRef w -> IORef a -> IO ()
updateInput appRef backendRef = do
  input <- liftM (^.appInput) (readIORef appRef)
  let keyTable    = input^.inputKeyTable
      buttonTable = input^.inputButtonTable
      listeners   = input^.inputListeners
      pos1        = input^.inputMousePos1
      pos2        = input^.inputMousePos2

  t <- B.elapsedTime backendRef
  updateInputTable t keyTable
  updateInputTable t buttonTable

  H.mapM_ (notifyKeyListeners listeners) keyTable
  H.mapM_ (notifyButtonListeners listeners pos1 pos2) buttonTable

  modifyIORef appRef $ appInput.inputMousePos1 .~ pos2

notifyKeyListeners :: [Registrable] -> (Key, InputState) -> IO ()
notifyKeyListeners listeners (key, state) =
  case state of
    Down ->
      mapM_ (\(MkInputListener a) -> keyPressed a key) listeners
    Up ->
      mapM_ (\(MkInputListener a) -> keyReleased a key) listeners
    Held _ dt ->
      mapM_ (\(MkInputListener a) -> keyHeld a key dt) listeners

notifyButtonListeners :: [Registrable] -> (Double, Double) -> (Double, Double)
                      -> (MouseButton, InputState) -> IO ()
notifyButtonListeners listeners
                      (posX1,posY1) pos2@(posX2,posY2) (button, state) =
  let pos' = (posX2-posX1, posY2-posY1) in
  case state of
    Down ->
      mapM_ (\(MkInputListener a) -> mouseClicked a button pos2) listeners
    Up ->
      mapM_ (\(MkInputListener a) -> mouseReleased a button pos2) listeners
    Held _ dt -> do
      mapM_ (\(MkInputListener a) -> mouseClickHeld a button dt pos2) listeners
      mapM_ (\(MkInputListener a) -> mouseClickDragged a button dt pos') listeners

-- | Upgrades Down to held, increments time on held, and does nothing when up.
updateInputState :: Double-> (k, InputState) -> IO (k, InputState)
updateInputState t (k, state) =
  case state of
    Down      -> return (k, Held t 0)
    Held t1 _ -> return (k, Held t1 (t-t1))
    Up        -> return (k, state)

-- | Updates the states of an input table
updateInputTable :: (Hashable k, Eq k)
                 => Double -> InputTable k -> IO ()
updateInputTable t table =
  H.toList table >>= mapM_ (updateInputState t >=> uncurry (H.insert table))


-- Input callbacks to communicate with backend --------------------------------
updateKeyboardInput :: B.Backend a => AppRef w -> IORef a -> Key -> B.InputState -> IO ()
updateKeyboardInput appRef backendRef key state = do
  input <- liftM (^.appInput) (readIORef appRef)
  let listeners = input^.inputListeners
  maybeState' <- cacheInput (input^.inputKeyTable) key state
  case maybeState' of
    Nothing -> return ()
    Just state' -> notifyKeyListeners listeners (key, state')

updateMouseMoveInput :: B.Backend a => AppRef w -> IORef a
                        -> Double -> Double -> IO ()
updateMouseMoveInput appRef _ posX2 posY2 = do
  input <- liftM (^.appInput) (readIORef appRef)
  let listeners = input^.inputListeners
      pos1@(posX1, posY1) = input^.inputMousePos2
      pos2 = (posX2, posY2)
      pos' = (posX2 - posX1, posY2 - posY1)

  modifyIORef appRef $ appInput.inputMousePos1 .~ pos1
  modifyIORef appRef $ appInput.inputMousePos2 .~ pos2

  mapM_ (\(MkInputListener a) -> mousePositioned a pos2) listeners
  mapM_ (\(MkInputListener a) -> mouseMoved a pos') listeners

updateMouseClickInput :: B.Backend a => AppRef w -> IORef a
                        -> MouseButton -> B.InputState
                        -> Double -> Double -> IO ()
updateMouseClickInput appRef backendRef button state posX2 posY2 = do
  input <- liftM (^.appInput) (readIORef appRef)
  let listeners = input^.inputListeners
      pos1 = input^.inputMousePos2
      pos2 = (posX2, posY2)

  modifyIORef appRef $ appInput.inputMousePos1 .~ pos1
  modifyIORef appRef $ appInput.inputMousePos2 .~ pos2

  maybeState' <- cacheInput (input^.inputButtonTable) button state
  case maybeState' of
    Nothing -> return ()
    Just state' -> notifyButtonListeners listeners pos1 pos2 (button, state')

updateScrolledInput :: B.Backend a => AppRef w -> IORef a
                        -> Double -> Double -> IO ()
updateScrolledInput appRef _ scrollX scrollY = do
  -- would be nice if this could be velocity
  listeners <- liftM (^.appInput.inputListeners) (readIORef appRef)
  mapM_ (\(MkInputListener a) -> scrolled a scrollX scrollY) listeners
