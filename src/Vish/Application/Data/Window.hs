module Vish.Application.Data.Window where

import Control.Lens

data WindowState = Windowed | FullScreen

data Window = Window
  { _winName :: String,
    _winX :: Int,
    _winY :: Int,
    _winW :: Int,
    _winH :: Int,
    _winState :: WindowState
  }

makeLenses ''Window
