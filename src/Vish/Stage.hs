module Vish.Stage
  ( module Vish.Stage
  , module Vish.Data.Stage
  )
where

import Vish.Data.Stage

import Vish.Graphics.Image (Image (..))
import qualified Vish.Graphics.Image as Img
import Vish.Graphics.Texture (Texture (..))

import Vish.Math.Vector (Vector2f (..))
import qualified Vish.Math.Vector as Vec

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

calcActorSize :: Stage -> Vector2f -> Vector2f
calcActorSize stage (Vector2f imgW imgH) =
  Vector2f imgW' imgH'
  where
    imgW' = imgH' * res
    imgH' = fromIntegral stageH - (2 * padV)
    res = imgW / imgH
    padV = fromIntegral stageH / 8
    (_, stageH) = stage ^. stageSize

calcCenterPos :: Stage -> Vector2f -> Vector2f
calcCenterPos stage imgSize =
  (stageSize' - imgSize) / 2
  where
    stageSize' = Vec.fromTuple $ (stage ^. stageSize) & both %~ fromIntegral
