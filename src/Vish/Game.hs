module Vish.Game where

import Vish.Script
import Vish.Application.App
import Vish.Application.Data.App

import Vish.Graphics.Data.Picture (Picture)
import qualified Vish.Graphics.Data.Picture as Pic

import Control.Lens

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
  appDraw app =
    return (app^.appWorld.currPic, app)

runScript :: Script -> IO ()
runScript = play . mkGameWorld
