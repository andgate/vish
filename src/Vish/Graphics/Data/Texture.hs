module Vish.Graphics.Data.Texture where

import Linear.V2 (V2 (..))
import qualified Linear.V2 as Vec

import qualified Data.HashTable.IO as H
import qualified Graphics.Rendering.OpenGL.GL as GL

data BitmapData = BitmapData Int

data Texture
        = Texture
        { texPath   :: String
        , texSize   :: V2 Float
        , texObject :: GL.TextureObject
        }


type TexCache = H.BasicHashTable String Texture
