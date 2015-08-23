module Vish.Data.Stage where

import Vish.MessageBox (MessageBox, mkMsgBox)
import Vish.Graphics.Image (Image (..))
import qualified Vish.Graphics.Image as Img

import Linear.V2 (V2 (..))
import qualified Linear.Vector as Vec

import Vish.Graphics.Font (Font)

import Control.Lens

data Stage = Stage
  { _stageSize        :: V2 Int
  , _stageBackground  :: Image
  , _stageLeft        :: Image
  , _stageCenter      :: Image
  , _stageRight       :: Image
  , _stageMsgBox      :: MessageBox
  }

mkStage :: Font -> IO Stage
mkStage fnt = do
  blankImg  <- Img.blank
  msgBox    <- mkMsgBox fnt
  return $ Stage
    { _stageSize        = Vec.zero
    , _stageBackground  = blankImg
    , _stageLeft        = blankImg
    , _stageCenter      = blankImg
    , _stageRight       = blankImg
    , _stageMsgBox      = msgBox
    }

makeLenses ''Stage
