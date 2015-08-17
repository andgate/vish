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
import Control.Arrow

import System.IO.Unsafe

draw :: Stage -> IO ()
draw stage =
  let imgs =
        [ stage ^. stageBackground
        , stage ^. stageLeft
        , stage ^. stageRight
        , stage ^. stageCenter
        ]
      size = stage ^. stageSize
  in Img.drawAll size imgs

resize :: V2 Int -> Stage -> Stage
resize scrnSize stg =
  let bgImg   = stg ^. stageBackground
      lImg    = stg ^. stageLeft
      rImg    = stg ^. stageRight
      cntrImg = stg ^. stageCenter
  in
    setBackground bgImg
      . setLeft lImg
      . setRight rImg
      . setCenter cntrImg
      . setSize scrnSize
      $ stg

setSize :: V2 Int -> Stage -> Stage
setSize = (stageSize .~)

setBackground :: Image -> Stage -> Stage
setBackground Blank stage = stage
setBackground img stage =
  let tex = imageTexture img
      pos = Vec.zero
      size = fromIntegral <$> stage ^. stageSize
      img' = Img.mkImageXYWH tex pos size
  in stage & stageBackground .~ img'

setLeft :: Image -> Stage -> Stage
setLeft Blank stage = stage
setLeft img stage =
  let tex = imageTexture img
      size = texSize . imageTexture $ img
      size' = calcActorSize stage size
      pos = calcCenterPos stage size'

      img' = Img.mkImageXYWH tex pos size'
  in stage & stageCenter .~ img'

setRight :: Image -> Stage -> Stage
setRight Blank stage = stage
setRight img stage =
  let tex = imageTexture img
      size = texSize . imageTexture $ img
      size' = calcActorSize stage size
      pos = calcCenterPos stage size'

      img' = Img.mkImageXYWH tex pos size'
  in stage & stageCenter .~ img'

setCenter :: Image -> Stage -> Stage
setCenter Blank stage = stage
setCenter img stage =
  let tex = imageTexture img
      size = texSize . imageTexture $ img
      size' = calcActorSize stage size
      pos = calcCenterPos stage size'

      img' = Img.mkImageXYWH tex pos size'
  in stage & stageCenter .~ img'

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
  (stgSize - imgSize) / 2
  where
    stgSize = fromIntegral <$> (stage ^. stageSize)
