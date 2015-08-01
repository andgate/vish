module Vish.Application.Data.App where

import Control.Lens
import Data.IORef

import Vish.Application.Data.Input (Input, mkInput)

import Vish.Graphics.Data.Picture
import Vish.Graphics.Data.Texture
import Vish.Graphics.Texture

type AppRef w = IORef (App w)

class AppListener w where
  appCreate :: AppRef w -> IO ()
  appCreate _ = return ()

  appResize :: AppRef w -> Int -> Int -> IO ()
  appResize a _ _ = return ()

  appUpdate :: AppRef w -> IO ()
  appUpdate _ = return ()

  appDraw :: AppRef w -> IO Picture
  appDraw _ = return blank

  appPostUpdate :: AppRef w -> IO ()
  appPostUpdate _ = return ()

  appPause :: AppRef w -> IO ()
  appPause _ = return ()

  appHide :: AppRef w -> IO ()
  appHide _ = return ()

  appDispose :: AppRef w -> IO ()
  appDispose _ = return ()

data App w = App
  { _appGfx :: Gfx
  , _appInput :: Input
  , _appListener :: w
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
