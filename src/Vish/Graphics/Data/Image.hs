module Vish.Graphics.Data.Image where

import Vish.Graphics.Data.Texture

import Linear.V2 (V2 (..))
import qualified Linear.V2 as Vec

data Image =
  Blank
  | Image
    { imageTexture :: Texture
    , imagePosition :: V2 Float
    , imageSize :: V2 Float
    }
