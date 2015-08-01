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
  appDraw appRef =
    liftM (^.appListener.currPic) (readIORef appRef)

  appDispose app =
    print "Disposing app"

runScript :: Script -> IO ()
runScript = play . mkGameWorld
