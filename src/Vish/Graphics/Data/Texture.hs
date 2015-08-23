module Vish.Graphics.Data.Texture where

import Control.Lens
import Data.IORef
import Linear.V2 (V2 (..))
import qualified Linear.Vector as Vec
import qualified Graphics.Rendering.OpenGL.GL as GL

data Texture
  = Texture
  { _path     :: String
  , _srcSize  :: V2 Double
  , _position :: V2 Double
  , _size     :: V2 Double
  , _object   :: IORef (Maybe GL.TextureObject)
  }

blank :: IO (Texture)
blank = do
  objRef <- newIORef Nothing
  return $ Texture
    { _path     = ""
    , _srcSize  = Vec.zero
    , _position = Vec.zero
    , _size     = Vec.zero
    , _object   = objRef
    }

makeLenses ''Texture
