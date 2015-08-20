module Vish.Graphics.Data.Texture where

import Linear.V2 (V2 (..))
import qualified Graphics.Rendering.OpenGL.GL as GL

data Texture
  = Texture
  { texturePath   :: String
  , textureSize   :: V2 Float
  , textureObject :: GL.TextureObject
  }
