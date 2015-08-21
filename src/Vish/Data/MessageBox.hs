module Vish.Data.MessageBox where

import qualified Vish.Graphics.Data.Color as C

import Vish.Graphics.Image (Image (..))
import qualified Vish.Graphics.Image as Img

import Vish.Graphics.Font (Font)
import qualified Vish.Graphics.Font as Font

import Linear.V2 (V2 (..))
import qualified Linear.Vector as Vec

import Control.Lens

data MessageBox =
  MessageBox
  { _msgBoxContent   :: String
  , _msgBoxBg        :: Image
  , _msgBoxImg       :: Image
  , _msgBoxPosition  :: V2 Float
  , _msgBoxSize      :: V2 Float
  , _msgBoxFontStyle :: Font.Style
  }

mkMsgBox :: Font -> IO MessageBox
mkMsgBox fnt = do
  blankImg <- Img.blank
  return $ MessageBox
    { _msgBoxFontStyle = Font.Style fnt C.black 12
    , _msgBoxContent   = ""
    , _msgBoxBg        = blankImg
    , _msgBoxImg       = blankImg
    , _msgBoxPosition  = Vec.zero
    , _msgBoxSize      = Vec.zero
    }

makeLenses ''MessageBox
