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
import Vish.Graphics.Texture (Texture)
import qualified Vish.Graphics.Texture as Tex

import Linear.V2 (V2 (..))
import qualified Linear.V2 as Vec

import Control.Lens
import Control.Monad

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
  stg' <- setBackground bgImg
       <=< setLeft lImg
       <=< setRight rImg
       <=< setCenter cntrImg
       .   setSize scrnSize
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
  Img.unload (stg ^. stageMsgBox . msgBoxBg)
  Img.unload (stg ^. stageMsgBox . msgBoxImg)
  blankImg   <- Img.blank
  stg & return
      . (stageMsgBox . msgBoxBg .~ blankImg)
      . (stageMsgBox . msgBoxImg .~ blankImg)


setBackground :: Image -> Stage -> IO Stage
setBackground img stg =  do
  let maxMeas = fromIntegral <$> stg ^. stageSize
      elemMeas = img ^. Img.texture . Tex.srcSize

      elemMeas' = LO.calcFillCropSize maxMeas elemMeas
      elemPos = LO.alignCenter maxMeas elemMeas'
      img' = img & (Img.position .~ elemPos)
                 . (Img.size .~ elemMeas')
  stg & return . (stageBackground .~ img')

clearBackground :: Stage -> IO Stage
clearBackground stg = do
  Img.unload (stg ^. stageBackground)
  blankImg   <- Img.blank
  stg & return
      . (stageBackground .~ blankImg)


setLeft :: Image -> Stage -> IO Stage
setLeft img stg = do
  let maxMeas = fromIntegral <$> stg ^. stageSize
      halfMaxMeas = maxMeas & Vec._x //~ 2
      elemMeas = img ^. Img.texture . Tex.srcSize

      elemMeas' = LO.calcFitSize halfMaxMeas elemMeas
      elemPos = LO.alignBottomCenter halfMaxMeas elemMeas'
      img' = img & (Img.position .~ elemPos)
                 . (Img.size .~ elemMeas')
  stg & return . (stageLeft .~ img')

setRight :: Image -> Stage -> IO Stage
setRight img stg = do
  let maxMeas = fromIntegral <$> stg ^. stageSize
      halfMaxMeas = maxMeas & Vec._x //~ 2
      elemMeas = img ^. Img.texture . Tex.srcSize

      elemMeas' = LO.calcFitSize halfMaxMeas elemMeas
      elemPos = LO.alignBottomCenter halfMaxMeas elemMeas'
      elemPos' = elemPos & Vec._x +~ (halfMaxMeas ^. Vec._x)
      img' = img & (Img.position .~ elemPos')
                 . (Img.size .~ elemMeas')
  stg & return . (stageRight .~ img')

setCenter :: Image -> Stage -> IO Stage
setCenter img stg = do
  let maxMeas = fromIntegral <$> stg ^. stageSize
      elemMeas = img ^. Img.texture . Tex.srcSize

      elemMeas' = LO.calcFitSize maxMeas elemMeas
      elemPos = LO.alignBottomCenter maxMeas elemMeas'
      img' = img & (Img.position .~ elemPos)
                 . (Img.size .~ elemMeas')
  stg & return . (stageCenter .~ img')

clearActors :: Stage -> IO Stage
clearActors stg = do
  Img.unload (stg ^. stageLeft)
  Img.unload (stg ^. stageRight)
  Img.unload (stg ^. stageCenter)
  blankImg   <- Img.blank
  stg & return
      . (stageLeft .~ blankImg)
      . (stageRight .~ blankImg)
      . (stageCenter .~ blankImg)
