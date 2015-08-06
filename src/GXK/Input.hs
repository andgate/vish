{-# LANGUAGE ExistentialQuantification #-}
module Vish.Application.Input
  ( module Vish.Application.Input
  , module Vish.Application.Data.Input
  )
where

import Vish.Application.Data.App
import Vish.Application.Data.Input

import Vish.Application.Internal.Data.Input

import Control.Lens
import Data.IORef
import Vish.Application.Data.IORef.Lens

registerInputListener :: InputListener l => AppRef w -> l -> IO ()
registerInputListener appRef listener = do
  let reg = register listener
  appRef & appInput.inputListeners @%~ (reg:)
