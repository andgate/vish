module Vish.Graphics.Data.Font where

import qualified Vish.Graphics.Data.Color as C

import Linear.V2 (V2 (..))
import qualified Linear.V2 as Vec
import qualified Linear.Vector as Vec

import Graphics.Text.TrueType (Font)

import Control.Lens

data Style =
  Style
    { _font  :: Font
    , _color :: C.Color
    , _pixelSize   :: Float
    }

makeLenses ''Style
