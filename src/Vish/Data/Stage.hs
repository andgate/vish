module Vish.Data.Stage where

import Vish.Graphics.Image (Image (..))
import qualified Vish.Graphics.Image as Img

import Vish.Math.Vector (Vector2f (..))
import qualified Vish.Math.Vector as Vec

import Control.Lens

data Stage = Stage
 { _stageSize :: (Int, Int)
 , _stageBackground :: Image
 , _stageLeft :: Image
 , _stageCenter :: Image
 , _stageRight :: Image
 }

emptyStage :: Stage
emptyStage =
 Stage
   { _stageSize = (0,0)
   , _stageBackground = Img.Blank
   , _stageLeft = Img.Blank
   , _stageCenter = Img.Blank
   , _stageRight = Img.Blank
   }

makeLenses ''Stage
