module Vish.Data.Game where

import Vish.Script
import Vish.Stage (Stage (..), mkStage)
import qualified Vish.Stage as Stage

import Vish.Graphics.Font (Font)
import Vish.Graphics.Texture

import Control.Lens
import Data.IORef
import qualified Data.List.Zipper as Z

data GameWorld = GameWorld
  { _gameCommands :: Z.Zipper ScriptCommand
  , _gameStage :: Stage
  , _gameWaiting :: Bool
  , _gameTexCache :: TexCache
  }

mkGameWorld :: Script -> Font -> IO GameWorld
mkGameWorld script fnt = do
  texCache <- mkTexCache
  let script' = scriptToZipper script
  return GameWorld
      { _gameCommands = script'
      , _gameStage = Stage.mkStage fnt
      , _gameWaiting = False
      , _gameTexCache = texCache
    }

makeLenses ''GameWorld
