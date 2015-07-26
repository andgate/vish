module Vish.Backend.Data.App where

import Control.Lens

import Vish.Graphics.Data.Picture
import Vish.Graphics.Data.Texture
import Vish.Graphics.Texture

class World w where
  worldUpdate :: w -> IO w
  worldDraw :: w -> IO (Picture, w)
  worldPostUpdate :: w -> IO w

data App w = App
  { _appGfx :: Gfx,
    _appWorld :: w
  }

data Gfx = Gfx
  { _gfxTexCache :: TexCache
  }

makeLenses ''App
makeLenses ''Gfx

mkApp :: World w => w -> IO (App w)
mkApp w = do
  gfx <- mkGfx
  return $ App gfx w

mkGfx :: IO Gfx
mkGfx = do
  texCache <- mkTexCache
  return $ Gfx texCache
