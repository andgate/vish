{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveGeneric #-}
module Vish.Application.Data.Input
  ( module Vish.Application.Data.InputPrims
  , Registrable (..)
  , register
  , InputListener (..)
  , KeyTable
  , Input (..)
  , inputKeyTable
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
  keyUp :: a -> Key -> IO ()
  keyUp _ _ = return ()
  keyDown :: a -> Key -> IO ()
  keyDown _ _ = return ()

type KeyTable = H.BasicHashTable Key KeyState

data Input = Input
  { _inputKeyTable :: KeyTable
  , _inputListeners :: [Registrable]
  }

makeLenses ''Input

mkInput :: IO Input
mkInput = do
  keytable <- H.new
  return $ Input keytable []
