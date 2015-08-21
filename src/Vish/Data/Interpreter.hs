module Vish.Data.Interpreter where

import Vish.Script (Script, ScriptCommand)
import qualified Vish.Script as S
import Vish.Stage (Stage, mkStage)
import Vish.Graphics.Font (Font)

--import GXK.App
import GXK.Data.App

import Control.Lens
import qualified Data.List.Zipper as Z


data InterpreterInput = InterpreterInput (AppRef Interpreter)

data Interpreter = Interpreter
  { _interpreterCommands :: Z.Zipper ScriptCommand
  , _interpreterStage :: Stage
  , _interpreterWaiting :: Bool
  }

mkInterpreter :: Script -> Font -> IO Interpreter
mkInterpreter script fnt = do
  let script' = S.scriptToZipper script
  stg <- mkStage fnt
  return Interpreter
      { _interpreterCommands = script'
      , _interpreterStage = stg
      , _interpreterWaiting = False
    }

makeLenses ''Interpreter
