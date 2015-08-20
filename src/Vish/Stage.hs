module Vish.Stage
  ( module Vish.Stage
  , module Vish.Data.Stage
  )
where

import Vish.Data.Stage

import qualified Vish.Layout as LO

import Vish.MessageBox (MessageBox, msgBoxBg, msgBoxImg)
import qualified Vish.MessageBox as MsgBox

import Vish.Graphics.Image (Image (..))
import qualified Vish.Graphics.Image as Img
import Vish.Graphics.Texture (Texture (..))

import Linear.V2 (V2 (..))
import qualified Linear.V2 as Vec

import Control.Lens

draw :: Stage -> IO ()
draw stg = do

  let msgBox = stg ^. stageMsgBox
      imgs =
        [ stg ^. stageBackground
        , stg ^. stageLeft
        , stg ^. stageRight
        , stg ^. stageCenter
        , msgBox ^. msgBoxBg
        , msgBox ^. msgBoxImg
        ]
      size = stg ^. stageSize
  Img.drawAll size imgs

resize :: V2 Int -> Stage -> IO Stage
resize scrnSize stg = do
  let bgImg   = stg ^. stageBackground
      lImg    = stg ^. stageLeft
      rImg    = stg ^. stageRight
      cntrImg = stg ^. stageCenter
      msgBox = stg ^. stageMsgBox
      stg' =
        setBackground bgImg
          . setLeft lImg
          . setRight rImg
          . setCenter cntrImg
          . setSize scrnSize
          $ stg
  setMsgBox msgBox stg'

setSize :: V2 Int -> Stage -> Stage
setSize = (stageSize .~)

setMsgBox :: MessageBox -> Stage -> IO Stage
setMsgBox msgBox stg = do
  let stgSize = stg ^. stageSize
  msgBox' <- MsgBox.resize stgSize msgBox
  stg & return . (stageMsgBox .~ msgBox')

setMessage :: String -> Stage -> IO Stage
setMessage msg stg = do
  let msgBox = stg ^. stageMsgBox
  msgBox' <- MsgBox.setMessage msg msgBox
  stg & return . (stageMsgBox .~ msgBox')

clearMessage :: Stage -> IO Stage
clearMessage stg = do
  msgBoxBg'  <- Img.delete (stg ^. stageMsgBox . msgBoxBg)
  msgBoxImg' <- Img.delete (stg ^. stageMsgBox . msgBoxImg)
  stg & return
      . (stageMsgBox . msgBoxBg .~ msgBoxBg')
      . (stageMsgBox . msgBoxImg .~ msgBoxImg')


setBackground :: Image -> Stage -> Stage
setBackground Blank stg = stg
setBackground img stg =
  let maxSize = fromIntegral <$> stg ^. stageSize
      elemSize = textureSize $ imageTexture img

      tex = imageTexture img
      elemSize' = LO.calcFillCropSize maxSize elemSize
      elemPos = LO.alignCenter maxSize elemSize'

      img' = Img.mkImageXYWH tex elemPos elemSize'
  in stg & stageBackground .~ img'

clearBg :: Stage -> IO Stage
clearBg stg = do
  bg' <- Img.delete (stg ^. stageBackground)
  stg & return
      . (stageBackground .~ bg')

setLeft :: Image -> Stage -> Stage
setLeft Blank stg = stg
setLeft img stg =
  let maxSize = fromIntegral <$> stg ^. stageSize
      halfMaxSize = maxSize & Vec._x //~ 2
      elemSize = textureSize $ imageTexture img

      tex = imageTexture img
      elemSize' = LO.calcFitSize halfMaxSize elemSize
      elemPos = LO.alignBottomCenter halfMaxSize elemSize'

      img' = Img.mkImageXYWH tex elemPos elemSize'
  in stg & (stageLeft .~ img')
         . (stageCenter .~ Img.Blank)

setRight :: Image -> Stage -> Stage
setRight Blank stg = stg
setRight img stg =
  let maxSize = fromIntegral <$> stg ^. stageSize
      halfMaxSize = maxSize & Vec._x //~ 2
      elemSize = textureSize $ imageTexture img

      tex = imageTexture img
      elemSize' = LO.calcFitSize halfMaxSize elemSize
      elemPos = LO.alignBottomCenter halfMaxSize elemSize'
      elemPos' = elemPos & Vec._x +~ (halfMaxSize ^. Vec._x)

      img' = Img.mkImageXYWH tex elemPos' elemSize'
  in stg & (stageRight .~ img')
         . (stageCenter .~ Img.Blank)

setCenter :: Image -> Stage -> Stage
setCenter Blank stg = stg
setCenter img stg =
  let maxSize = fromIntegral <$> stg ^. stageSize
      elemSize = textureSize $ imageTexture img

      tex = imageTexture img
      elemSize' = LO.calcFitSize maxSize elemSize
      elemPos = LO.alignBottomCenter maxSize elemSize'

      img' = Img.mkImageXYWH tex elemPos elemSize'
  in stg & (stageCenter .~ img')
         . (stageLeft .~ Img.Blank)
         . (stageRight .~ Img.Blank)
