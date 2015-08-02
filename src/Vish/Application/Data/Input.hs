{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveGeneric #-}
module Vish.Application.Data.Input
  ( module Vish.Application.Data.InputPrims
  , module Vish.Application.Data.Input
  )
where

import Vish.Application.Data.InputPrims

import Control.Lens
import Data.IORef
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.HashTable.IO as H

data Registrable = forall a . InputListener a => MkInputListener a

register :: InputListener a => a -> Registrable
register = MkInputListener

class InputListener a where
  keyReleased :: a -> Key -> IO ()
  keyReleased _ _ = return ()

  keyPressed :: a -> Key -> IO ()
  keyPressed _ _ = return ()

  keyHeld :: a -> Key -> Double -> IO ()
  keyHeld _ _ dt = return ()

  keyTyped :: a -> Char -> IO ()
  keyTyped _ _ = return ()

  mousePositioned :: a -> (Double, Double) -> IO ()
  mousePositioned _ (posX, posY) = return ()

  mouseMoved :: a -> (Double, Double) -> IO ()
  mouseMoved _ (velX, velY) = return ()

  mouseReleased :: a -> MouseButton -> (Double, Double) ->  IO ()
  mouseReleased _ _ (posX, posY) = return ()

  mouseClicked :: a -> MouseButton -> (Double, Double) -> IO ()
  mouseClicked _ _ (posX, posY) = return ()

  mouseClickHeld :: a -> MouseButton -> Double -> (Double, Double) -> IO ()
  mouseClickHeld _ _ dt (posX, posY) = return ()

  mouseClickDragged :: a -> MouseButton -> Double -> (Double, Double) -> IO ()
  mouseClickDragged _ _ dt (velX, velY) = return ()

  scrolled :: a -> Double -> Double -> IO ()
  scrolled _ _ _ = return ()

type InputTable k  = H.BasicHashTable k InputState
type KeyTable      = InputTable Key
type ButtonTable   = InputTable MouseButton

data Input = Input
  { _inputKeyTable :: KeyTable
  , _inputButtonTable :: ButtonTable
  , _inputListeners :: [Registrable]
  , _inputMousePos1 :: (Double, Double)
  , _inputMousePos2 :: (Double, Double)
  }

makeLenses ''Input

mkInput :: IO Input
mkInput = do
  keyTable <- H.new
  buttonTable <- H.new
  let listeners = []
      mousePos = (0,0)
  return
    Input
    { _inputKeyTable = keyTable
    , _inputButtonTable = buttonTable
    , _inputListeners = listeners
    , _inputMousePos1 = mousePos
    , _inputMousePos2 = mousePos
    }
