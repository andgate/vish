module Vish.Data.App where

import Control.Lens

import Vish.Graphics.Data.Picture
import Vish.Graphics.Data.Texture
import Vish.Graphics.Texture

class AppListener w where
  appStart :: App w -> IO (App w) 
  appUpdate :: App w -> IO (App w)
  appDraw :: App w -> IO (Picture, App w)
  appPostUpdate :: App w -> IO (App w)

data App w = App
  { _appGfx :: Gfx,
    _appWorld :: w
  }

data Gfx = Gfx
  { _gfxTexCache :: TexCache
  }

makeLenses ''App
makeLenses ''Gfx

mkApp :: AppListener w => w -> IO (App w)
mkApp world = do
  gfx <- mkGfx
  return $ App gfx world

mkGfx :: IO Gfx
mkGfx = do
  texCache <- mkTexCache
  return $ Gfx texCache
