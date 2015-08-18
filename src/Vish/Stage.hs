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
  let maxSize = fromIntegral <$> stage ^. stageSize
      elemSize = texSize $ imageTexture img

      tex = imageTexture img
      elemSize' = calcFillCropSize maxSize elemSize
      elemPos = alignCenter maxSize elemSize'

      img' = Img.mkImageXYWH tex elemPos elemSize'
  in stage & stageBackground .~ img'

setLeft :: Image -> Stage -> Stage
setLeft Blank stage = stage
setLeft img stage =
  let maxSize = fromIntegral <$> stage ^. stageSize
      halfMaxSize = maxSize & Vec._x //~ 2
      elemSize = texSize $ imageTexture img

      tex = imageTexture img
      elemSize' = calcFitSize halfMaxSize elemSize
      elemPos = alignBottomCenter halfMaxSize elemSize'

      img' = Img.mkImageXYWH tex elemPos elemSize'
  in stage & stageLeft .~ img'

setRight :: Image -> Stage -> Stage
setRight Blank stage = stage
setRight img stage =
  let maxSize = fromIntegral <$> stage ^. stageSize
      halfMaxSize = maxSize & Vec._x //~ 2
      elemSize = texSize $ imageTexture img

      tex = imageTexture img
      elemSize' = calcFitSize halfMaxSize elemSize
      elemPos = alignBottomCenter halfMaxSize elemSize'
      elemPos' = elemPos & Vec._x +~ (halfMaxSize ^. Vec._x)

      img' = Img.mkImageXYWH tex elemPos' elemSize'
  in stage & stageRight .~ img'

setCenter :: Image -> Stage -> Stage
setCenter Blank stage = stage
setCenter img stage =
  let maxSize = fromIntegral <$> stage ^. stageSize
      elemSize = texSize $ imageTexture img

      tex = imageTexture img
      elemSize' = calcFitSize maxSize elemSize
      elemPos = alignBottomCenter maxSize elemSize'

      img' = Img.mkImageXYWH tex elemPos elemSize'
  in stage & stageCenter .~ img'

calcFitSize :: V2 Float -> V2 Float -> V2 Float
calcFitSize (V2 maxW maxH) elemSize =
  if maxW > maxH
    then calcSizeByHeight maxH elemSize
    else calcSizeByWidth maxW elemSize

calcFillCropSize :: V2 Float -> V2 Float -> V2 Float
calcFillCropSize (V2 maxW maxH) (V2 elemW elemH) =
  if maxW > maxH
    then if maxH > elemH'
      then V2 elemW' maxH
      else V2 maxW elemH'
    else if maxW > elemW'
      then V2 maxW elemH'
      else V2 elemW' maxH

  where
    res = elemW / elemH
    elemW' = maxH * res
    elemH' = maxW / res

calcSizeByWidth :: Float -> V2 Float -> V2 Float
calcSizeByWidth maxW (V2 elemW elemH) =
  V2 elemW' elemH'
  where
    elemW' = maxW
    elemH' = elemW' * res
    res = elemH / elemW

calcSizeByHeight :: Float -> V2 Float -> V2 Float
calcSizeByHeight maxH (V2 elemW elemH) =
  V2 elemW' elemH'
  where
    elemW' = elemH' * res
    elemH' = maxH
    res = elemW / elemH

alignBottomCenter :: V2 Float -> V2 Float -> V2 Float
alignBottomCenter (V2 cellW cellH) (V2 elemW elemH) =
  V2 x y
  where
    x = (cellW - elemW) / 2
    y = cellH - elemH

alignCenter :: V2 Float -> V2 Float -> V2 Float
alignCenter cellSize elemSize =
  (cellSize - elemSize) / 2

alignBottom :: V2 Float -> V2 Float -> V2 Float
alignBottom (V2 _ cellY) (V2 _ elemY) =
  V2 0 (cellY - elemY)
