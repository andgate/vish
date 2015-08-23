module Vish.MessageBoxSkin
  ( module Vish.MessageBoxSkin
  , module Vish.Data.MessageBoxSkin
  )
where

import Vish.Data.MessageBoxSkin


import Vish.Graphics.Image (Image)
import qualified Vish.Graphics.Image as Img

import Vish.Graphics.ImageAtlas (ImageAtlas)
import qualified Vish.Graphics.ImageAtlas as ImgAtlas

import Control.Lens

blank :: IO MessageBoxSkin
blank = do
  blankImg <- Img.blank
  return $ MessageBoxSkin
    { _topLeft  = blankImg
    , _top      = blankImg
    , _topRight = blankImg

    , _left   = blankImg
    , _center = blankImg
    , _right  = blankImg

    , _bottomRight = blankImg
    , _bottom      = blankImg
    , _bottomLeft  = blankImg
    }

fromAtlas :: ImageAtlas -> MessageBoxSkin
fromAtlas ia =
  MessageBoxSkin
    { _topLeft  = ImgAtlas.grab ia "top_left"
    , _top      = ImgAtlas.grab ia "top"
    , _topRight = ImgAtlas.grab ia "top_right"

    , _left   = ImgAtlas.grab ia "left"
    , _center = ImgAtlas.grab ia "center"
    , _right  = ImgAtlas.grab ia "right"

    , _bottomRight = ImgAtlas.grab ia "bottom_right"
    , _bottom      = ImgAtlas.grab ia "bottom"
    , _bottomLeft  = ImgAtlas.grab ia "bottom_left"
    }

toList :: MessageBoxSkin -> [Image]
toList mbs =
  map (mbs &)
    [ (^.topLeft)
    , (^.top)
    , (^.topRight)

    , (^.left)
    , (^.center)
    , (^.right)

    , (^.bottomRight)
    , (^.bottom)
    , (^.bottomLeft)
    ]
