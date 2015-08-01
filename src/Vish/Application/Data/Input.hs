{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveGeneric #-}
module Vish.Application.Data.Input
  ( module Vish.Application.Data.InputPrims
  , Registrable (..)
  , register
  , InputListener (..)
  , Input (..)
  , KeyTable
  , ButtonTable
  , inputKeyTable
  , inputButtonTable
  , inputListeners
  , mkInput
  )
where

import Vish.Application.Data.InputPrims

import Control.Lens
import Data.IORef
import qualified Data.HashTable.IO as H

data Registrable = forall a . InputListener a => MkInputListener a

register :: InputListener a => a -> Registrable
register = MkInputListener

class InputListener a where
  keyReleased :: a -> Key -> IO ()
  keyReleased _ _ = return ()

  keyPressed :: a -> Key -> IO ()
  keyPressed _ _ = return ()

  keyHeld :: a -> Key -> IO ()
  keyHeld _ _ = return ()

  keyTyped :: a -> Char -> IO ()
  keyTyped _ _ = return ()

  mouseMoved :: a -> Double -> Double -> IO ()
  mouseMoved _ _ _ = return ()

  mouseButtonReleased :: a -> MouseButton -> IO ()
  mouseButtonReleased _ _ = return ()

  mouseButtonClicked :: a -> MouseButton -> IO ()
  mouseButtonClicked _ _ = return ()

  mouseButtonHeld :: a -> MouseButton -> IO ()
  mouseButtonHeld _ _ = return ()

  scrolled :: a -> Double -> Double -> IO ()
  scrolled _ _ _ = return ()

type KeyTable = H.BasicHashTable Key KeyState
type ButtonTable = H.BasicHashTable MouseButton KeyState

data Input = Input
  { _inputKeyTable :: KeyTable
  , _inputButtonTable :: ButtonTable
  , _inputListeners :: [Registrable]
  }

makeLenses ''Input

mkInput :: IO Input
mkInput = do
  keyTable <- H.new
  buttonTable <- H.new
  return $ Input keyTable buttonTable []
