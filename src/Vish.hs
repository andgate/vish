module Vish where

import qualified GXK.App as GXK

import Vish.Script
import Vish.Game
import qualified Vish.GameConfig as Cfg

play :: Script -> IO ()
play script =
  GXK.play =<< Cfg.toGame script =<< Cfg.load
