module Vish where

import Control.Lens

import Vish.Script
import Vish.Application.App
import Vish.Application.Data.App

import Vish.Graphics.Data.Picture (Picture)
import qualified Vish.Graphics.Data.Picture as Pic

data GameWorld = GameWorld
  { _gameScript :: Script,
    _currPic :: Picture
  }

mkGameWorld :: Script -> GameWorld
mkGameWorld script =
  GameWorld
  { _gameScript = script,
    _currPic = Pic.blank
  }

makeLenses ''GameWorld

instance AppListener GameWorld where
  appStart =
    return
  appUpdate =
    return
  appDraw app =
    return (app^.appWorld.currPic, app)
  appPostUpdate =
    return

runScript :: Script -> IO ()
runScript = play . mkGameWorld
