module Vish.Graphics.Data.Texture where

import qualified Data.HashTable.IO as H
import qualified Graphics.Rendering.OpenGL.GL as GL

data BitmapData = BitmapData Int

data Texture
        = Texture
        { texPath     :: String
        , texWidth    :: Int
        , texHeight   :: Int
        , texObject   :: GL.TextureObject
        }


type TexCache = H.BasicHashTable String Texture
