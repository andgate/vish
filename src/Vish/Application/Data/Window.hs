module Vish.Application.Data.Window where

import Control.Lens

data WindowState = WindowFloating | WindowFullscreen
  deriving (Show)

data Window = Window
  { _windowName :: String,
    _windowX :: Int,
    _windowY :: Int,
    _windowWidth :: Int,
    _windowHeight :: Int,
    _windowState :: WindowState
  }

windowDefault :: Window
windowDefault =
  Window
  { _windowName = "default",
    _windowX = 0,
    _windowY = 0,
    _windowWidth = 640,
    _windowHeight = 480,
    _windowState = WindowFloating
  }

makeLenses ''Window
