module Vish.Graphics.Data.Texture where

import Vish.Math.Data.Vector

import qualified Data.HashTable.IO as H
import qualified Graphics.Rendering.OpenGL.GL as GL

data BitmapData = BitmapData Int

data Texture
        = Texture
        { texPath   :: String
        , texSize   :: Vector2f
        , texObject :: GL.TextureObject
        }


type TexCache = H.BasicHashTable String Texture
