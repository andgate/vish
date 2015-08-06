{-# LANGUAGE Rank2Types #-}
module Vish.Application.Data.App where

import Control.Lens
import Data.IORef

import Vish.Application.Internal.Data.Input (mkInput, Input)
import Vish.Application.Data.Window

import Vish.Graphics.Data.Picture
import Vish.Graphics.Data.Texture
import Vish.Graphics.Texture

type AppRef w = IORef (App w)

getApp :: AppListener w => AppRef w -> IO (App w)
getApp = readIORef

class AppListener w where
  appCreate :: AppRef w -> IO ()
  appCreate _ = return ()

  appResize :: AppRef w -> Int -> Int -> IO ()
  appResize _ _ _ = return ()

  appUpdate :: AppRef w -> IO ()
  appUpdate _ = return ()

  appDraw :: AppRef w -> IO Picture
  appDraw _ = return blank

  appPostUpdate :: AppRef w -> IO ()
  appPostUpdate _ = return ()

  appPause :: AppRef w -> IO ()
  appPause _ = return ()

  appResume :: AppRef w -> IO ()
  appResume _ = return ()

  appDispose :: AppRef w -> IO ()
  appDispose _ = return ()

data App w = App
  { _appGfx :: Gfx
  , _appInput :: Input
  , _appWindow :: Window
  , _appWorld :: w
  , _appStatus :: AppStatus
  }

data AppStatus = AppPlay | AppQuit

data Gfx = Gfx
  { _gfxTexCache :: TexCache
  }

makeLenses ''App
makeLenses ''Gfx

mkApp :: AppListener w => w -> IO (App w)
mkApp world = do
  gfx       <- mkGfx
  input     <- mkInput
  let win = windowDefault
      status = AppPlay

  return $ App gfx input win world status

mkGfx :: IO Gfx
mkGfx = do
  texCache <- mkTexCache
  return $ Gfx texCache
