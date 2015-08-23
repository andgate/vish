module Vish.Data.MessageBoxSkin where

import Vish.Graphics.Image (Image)

import Control.Lens

data MessageBoxSkin =
  MessageBoxSkin
    { _topLeft :: Image
    , _top     :: Image
    , _topRight :: Image

    , _left   :: Image
    , _center :: Image
    , _right  :: Image

    , _bottomRight :: Image
    , _bottom      :: Image
    , _bottomLeft  :: Image
    }

makeLenses ''MessageBoxSkin
