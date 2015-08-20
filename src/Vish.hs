module Vish where

import qualified GXK.App as GXK

import Vish.Script
import qualified Vish.Config as Cfg

play :: Script -> IO ()
play script =
  GXK.play =<< Cfg.toInterpreter script =<< Cfg.load
