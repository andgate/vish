module Vish.Game where

import Vish.Script
import Vish.Application.App
import Vish.Application.Data.App

import Vish.Graphics.Data.Picture (Picture)
import qualified Vish.Graphics.Data.Picture as Pic

import Control.Lens
import Control.Monad
import Data.IORef

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
    liftM (^.appWorld.currPic) (getApp app)

  appDispose _ =
    print "Disposing app"

  appPause _ =
    print "Application paused."

  appResume _ =
    print "Application resumed."

runScript :: Script -> IO ()
runScript = play . mkGameWorld
