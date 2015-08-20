module Vish.Graphics.Data.Texture where

import Linear.V2 (V2 (..))

import qualified Data.HashTable.IO as H
import qualified Graphics.Rendering.OpenGL.GL as GL

--data BitmapData = BitmapData Int

data Texture
  = Texture
  { texturePath   :: String
  , textureSize   :: V2 Float
  , textureObject :: GL.TextureObject
  }


type TexCache = H.BasicHashTable String Texture
