module Vish.Renderer.Data.Texture where

import Data.Word
import Data.IORef
import Foreign.ForeignPtr
import System.Mem.StableName
import qualified Graphics.Rendering.OpenGL.GL   as GL

data BitmapData = BitmapData Int

data Texture
        = Texture
        {
          texName       :: StableName BitmapData
        , texWidth      :: Int
        , texHeight     :: Int
        , texData       :: ForeignPtr Word8
        , texObject     :: GL.TextureObject
        , texCacheable    :: Bool
        }


type TexCache = IORef [Texture]

initTexCache :: IO TexCache
initTexCache = newIORef []
