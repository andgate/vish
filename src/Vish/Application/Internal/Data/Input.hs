{-# LANGUAGE ExistentialQuantification #-}
module Vish.Application.Internal.Data.Input where

import Vish.Application.Data.Input

import Control.Lens
import qualified Data.HashTable.IO as H

data Registrable = forall a . InputListener a => MkInputListener a

register :: InputListener a => a -> Registrable
register = MkInputListener

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
