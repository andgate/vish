module Vish.Stage
  ( module Vish.Stage
  , module Vish.Data.Stage
  )
where

import Vish.Data.Stage

import Vish.Graphics.Image (Image (..))
import qualified Vish.Graphics.Image as Img
import Vish.Graphics.Texture (Texture (..))

import Linear.V2 (V2 (..))
import qualified Linear.V2 as Vec
import qualified Linear.Vector as Vec

import Control.Lens

drawStage :: Stage -> IO ()
drawStage stage =
  let imgs =
        [ stage ^. stageBackground
        , stage ^. stageLeft
        , stage ^. stageRight
        , stage ^. stageCenter
        ]
      size = stage ^. stageSize
  in Img.drawImages size imgs

setStageBackground :: Texture -> Stage -> Stage
setStageBackground tex stage =
  let pos = Vec.zero
      size = texSize tex
      img = Img.mkImageXYWH tex pos size
  in stage & stageBackground .~ img

setStageCenter :: Texture -> Stage -> Stage
setStageCenter tex stage =
  let size = texSize tex
      size' = calcActorSize stage size
      pos = calcCenterPos stage size'

      img = Img.mkImageXYWH tex pos size'
  in stage & stageCenter .~ img

calcActorSize :: Stage -> V2 Float -> V2 Float
calcActorSize stage (V2 imgW imgH) =
  V2 imgW' imgH'
  where
    imgW' = imgH' * res
    imgH' = fromIntegral stageH - (2 * padV)
    res = imgW / imgH
    padV = fromIntegral stageH / 8
    V2 _ stageH = stage ^. stageSize

calcCenterPos :: Stage -> V2 Float -> V2 Float
calcCenterPos stage imgSize =
  (stageSize' - imgSize) / 2
  where
    stageSize' = fromIntegral <$> (stage ^. stageSize)
