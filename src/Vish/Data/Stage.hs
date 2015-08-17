module Vish.Data.Stage where

import Vish.Graphics.Image (Image (..))
import qualified Vish.Graphics.Image as Img

import Linear.V2 (V2 (..))
import qualified Linear.V2 as Vec
import qualified Linear.Vector as Vec

import Control.Lens

data Stage = Stage
 { _stageSize :: V2 Int
 , _stageBackground :: Image
 , _stageLeft :: Image
 , _stageCenter :: Image
 , _stageRight :: Image
 }

emptyStage :: Stage
emptyStage =
 Stage
   { _stageSize = Vec.zero
   , _stageBackground = Img.Blank
   , _stageLeft = Img.Blank
   , _stageCenter = Img.Blank
   , _stageRight = Img.Blank
   }

makeLenses ''Stage
