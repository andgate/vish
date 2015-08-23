module Vish.Data.MessageBox where

import qualified Vish.Graphics.Data.Color as C

import Vish.Graphics.Image (Image)
import qualified Vish.Graphics.Image as Img

import Vish.MessageBoxSkin (MessageBoxSkin)
import qualified Vish.MessageBoxSkin as Skin

import Vish.Graphics.Font (Font)
import qualified Vish.Graphics.Font as Font

import Linear.V2 (V2)
import qualified Linear.Vector as Vec

import Control.Lens

data MessageBox =
  MessageBox
  { _content   :: String
  , _position  :: V2 Double
  , _size      :: V2 Double
  , _fontStyle :: Font.Style
  , _skin      :: MessageBoxSkin
  , _text      :: Image
  }

mkMsgBox :: Font -> IO MessageBox
mkMsgBox fnt = do
  blankImg <- Img.blank
  blankSkin <- Skin.blank
  return $ MessageBox
    { _content   = ""
    , _position  = Vec.zero
    , _size      = Vec.zero
    , _fontStyle = Font.Style fnt C.black 12
    , _skin      = blankSkin
    , _text      = blankImg
    }

makeLenses ''MessageBox
