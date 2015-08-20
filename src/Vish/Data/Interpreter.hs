module Vish.Data.Interpreter where

import Vish.Script (Script, ScriptCommand)
import qualified Vish.Script as S
import Vish.Stage (Stage, mkStage)
import Vish.Graphics.Font (Font)
import qualified Vish.Graphics.Texture as Tex

--import GXK.App
import GXK.Data.App

import Control.Lens
import qualified Data.List.Zipper as Z


data InterpreterInput = InterpreterInput (AppRef Interpreter)

data Interpreter = Interpreter
  { _interpreterCommands :: Z.Zipper ScriptCommand
  , _interpreterStage :: Stage
  , _interpreterWaiting :: Bool
  , _interpreterTexCache :: Tex.TexCache
  }

mkInterpreter :: Script -> Font -> IO Interpreter
mkInterpreter script fnt = do
  texCache <- Tex.mkTexCache
  let script' = S.scriptToZipper script
  return Interpreter
      { _interpreterCommands = script'
      , _interpreterStage = mkStage fnt
      , _interpreterWaiting = False
      , _interpreterTexCache = texCache
    }

makeLenses ''Interpreter
