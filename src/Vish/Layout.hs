module Vish.Layout where

import Linear.V2 (V2 (..))

calcFitSize :: V2 Float -> V2 Float -> V2 Float
calcFitSize (V2 maxW maxH) elemSize =
  if maxW > maxH
    then calcSizeByHeight maxH elemSize
    else calcSizeByWidth maxW elemSize

calcFillCropSize :: V2 Float -> V2 Float -> V2 Float
calcFillCropSize (V2 maxW maxH) (V2 elemW elemH)
  | maxW > maxH =
      if maxH > elemH'
        then V2 elemW' maxH
        else V2 maxW elemH'
  | otherwise =
      if maxW > elemW'
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
