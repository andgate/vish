module Vish.Graphics.Data.Image where

import Control.Lens

import Vish.Graphics.Texture (Texture)
import qualified Vish.Graphics.Texture as Tex

import Linear.V2 (V2 (..))
import qualified Linear.Vector as Vec

data Image =
  Image
    { _texture  :: Texture
    , _position :: V2 Double
    , _size     :: V2 Double
    }

blank :: IO Image
blank = do
  blankTex <- Tex.blank
  return $ Image
    { _texture  = blankTex
    , _position = Vec.zero
    , _size     = Vec.zero
    }

makeLenses ''Image
