module Vish.Application.Data.App where

import Control.Lens

import Vish.Application.Data.Input (Input, mkInput)

import Vish.Graphics.Data.Picture
import Vish.Graphics.Data.Texture
import Vish.Graphics.Texture

class AppListener w where
  appStart :: App w -> IO (App w)
  appStart = return
  appUpdate :: App w -> IO (App w)
  appUpdate = return
  appDraw :: App w -> IO (Picture, App w)
  appDraw = return . (blank,)
  appPostUpdate :: App w -> IO (App w)
  appPostUpdate = return

data App w = App
  { _appGfx :: Gfx
  , _appInput :: Input
  , _appWorld :: w
  }

data Gfx = Gfx
  { _gfxTexCache :: TexCache
  }

makeLenses ''App
makeLenses ''Gfx

mkApp :: AppListener w => w -> IO (App w)
mkApp world = do
  gfx <- mkGfx
  input <- mkInput
  return $ App gfx input world

mkGfx :: IO Gfx
mkGfx = do
  texCache <- mkTexCache
  return $ Gfx texCache
