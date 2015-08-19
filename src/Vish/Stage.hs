module Vish.Stage
  ( module Vish.Stage
  , module Vish.Data.Stage
  )
where

import Vish.Data.Stage

import qualified Vish.Layout as LO

import Vish.MessageBox (MessageBox, msgBoxPosition, msgBoxSize, msgBoxBg, msgBoxImg)
import qualified Vish.MessageBox as MsgBox

import Vish.Graphics.Image (Image (..))
import qualified Vish.Graphics.Image as Img
import Vish.Graphics.Texture (Texture (..))

import Linear.V2 (V2 (..))
import qualified Linear.V2 as Vec
import qualified Linear.Vector as Vec

import Control.Lens
import Control.Arrow
import Control.Monad

import System.IO.Unsafe

draw :: Stage -> IO ()
draw stg = do

  let msgBox = stg ^. stageMsgBox
      imgs =
        [ stg ^. stageBackground
        , stg ^. stageLeft
        , stg ^. stageRight
        , stg ^. stageCenter
        , maybe Img.Blank (^. msgBoxBg) msgBox
        , maybe Img.Blank (^. msgBoxImg) msgBox
        ]
      size = stg ^. stageSize
  Img.drawAll size imgs

resize :: V2 Int -> Stage -> IO Stage
resize scrnSize stg = do
  let bgImg   = stg ^. stageBackground
      lImg    = stg ^. stageLeft
      rImg    = stg ^. stageRight
      cntrImg = stg ^. stageCenter
      maybeMsgBox = stg ^. stageMsgBox
      stg' =
        setBackground bgImg
          . setLeft lImg
          . setRight rImg
          . setCenter cntrImg
          . setSize scrnSize
          $ stg
  case maybeMsgBox of
    Nothing ->
      return stg'
    Just msgBox ->
      setMsgBox msgBox stg'

setSize :: V2 Int -> Stage -> Stage
setSize = (stageSize .~)

setMsgBox :: MessageBox -> Stage -> IO Stage
setMsgBox msgBox stg = do
  let stgSize = stg ^. stageSize
  msgBox' <- MsgBox.resize stgSize msgBox
  stg & return . (stageMsgBox .~ Just msgBox')

setMessage :: String -> Stage -> IO Stage
setMessage msg stg = do
  let maybeMsgBox = stg ^. stageMsgBox
  case maybeMsgBox of
    Nothing ->
      return stg
    Just msgBox -> do
      msgBox' <- MsgBox.setMessage msg msgBox
      stg & return . (stageMsgBox .~ Just msgBox')


setBackground :: Image -> Stage -> Stage
setBackground Blank stage = stage
setBackground img stage =
  let maxSize = fromIntegral <$> stage ^. stageSize
      elemSize = texSize $ imageTexture img

      tex = imageTexture img
      elemSize' = LO.calcFillCropSize maxSize elemSize
      elemPos = LO.alignCenter maxSize elemSize'

      img' = Img.mkImageXYWH tex elemPos elemSize'
  in stage & stageBackground .~ img'

setLeft :: Image -> Stage -> Stage
setLeft Blank stage = stage
setLeft img stage =
  let maxSize = fromIntegral <$> stage ^. stageSize
      halfMaxSize = maxSize & Vec._x //~ 2
      elemSize = texSize $ imageTexture img

      tex = imageTexture img
      elemSize' = LO.calcFitSize halfMaxSize elemSize
      elemPos = LO.alignBottomCenter halfMaxSize elemSize'

      img' = Img.mkImageXYWH tex elemPos elemSize'
  in stage & stageLeft .~ img'

setRight :: Image -> Stage -> Stage
setRight Blank stage = stage
setRight img stage =
  let maxSize = fromIntegral <$> stage ^. stageSize
      halfMaxSize = maxSize & Vec._x //~ 2
      elemSize = texSize $ imageTexture img

      tex = imageTexture img
      elemSize' = LO.calcFitSize halfMaxSize elemSize
      elemPos = LO.alignBottomCenter halfMaxSize elemSize'
      elemPos' = elemPos & Vec._x +~ (halfMaxSize ^. Vec._x)

      img' = Img.mkImageXYWH tex elemPos' elemSize'
  in stage & stageRight .~ img'

setCenter :: Image -> Stage -> Stage
setCenter Blank stage = stage
setCenter img stage =
  let maxSize = fromIntegral <$> stage ^. stageSize
      elemSize = texSize $ imageTexture img

      tex = imageTexture img
      elemSize' = LO.calcFitSize maxSize elemSize
      elemPos = LO.alignBottomCenter maxSize elemSize'

      img' = Img.mkImageXYWH tex elemPos elemSize'
  in stage & stageCenter .~ img'
