module Vish.Graphics.Data.Image where

import Vish.Math.Vector (Vector2f (..))
import qualified Vish.Math.Vector as Vec
import Vish.Graphics.Data.Texture

data Image =
  Blank
  | Image
    { imageTexture :: Texture
    , imagePosition :: Vector2f
    , imageSize :: Vector2f
    }
