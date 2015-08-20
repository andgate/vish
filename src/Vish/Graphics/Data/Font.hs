module Vish.Graphics.Data.Font where

import qualified Vish.Graphics.Data.Color as C

import Graphics.Text.TrueType (Font)

import Control.Lens

data Style =
  Style
    { _font  :: Font
    , _color :: C.Color
    , _pixelSize   :: Float
    }

makeLenses ''Style
