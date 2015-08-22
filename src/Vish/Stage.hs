module Vish.Stage
  ( module Vish.Stage
  , module Vish.Data.Stage
  )
where

import Vish.Data.Stage

import qualified Vish.Layout as LO

import Vish.MessageBox (MessageBox)
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

  let mb = stg ^. stageMsgBox
      is =
        [ stg ^. stageBackground
        , stg ^. stageLeft
        , stg ^. stageRight
        , stg ^. stageCenter
        , mb  ^. MsgBox.text
        ]
      stgS = stg ^. stageSize
  Img.drawAll stgS is

layout :: V2 Int -> Stage -> IO Stage
layout sS stg =
  let bgImg   = stg ^. stageBackground
  in layoutMsgBox . layoutBackground  . layoutLeft . layoutRight . layoutCenter . (stageSize .~ sS) $ stg

setMsgBox :: MessageBox -> Stage -> IO Stage
setMsgBox msgBox stg = do
  let stgSize = stg ^. stageSize
  msgBox' <- MsgBox.layout stgSize msgBox
  stg & return . (stageMsgBox .~ msgBox')

layoutMsgBox :: Stage -> IO Stage
layoutMsgBox stg = do
  let mb   = stg ^. stageMsgBox
      stgS = stg ^. stageSize
  mb' <- MsgBox.layout stgS mb
  stg & return . (stageMsgBox .~ mb')

setMessage :: String -> Stage -> IO Stage
setMessage msg stg = do
  let msgBox = stg ^. stageMsgBox
  msgBox' <- MsgBox.setMessage msg msgBox
  stg & return . (stageMsgBox .~ msgBox')

clearMessage :: Stage -> IO Stage
clearMessage stg = do
  Img.unload (stg ^. stageMsgBox . MsgBox.text)
  blankImg   <- Img.blank
  stg & return
      . (stageMsgBox . MsgBox.text .~ blankImg)


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

layoutBackground :: Stage -> Stage
layoutBackground stg =
  let maxMeas = fromIntegral <$> stg ^. stageSize
      elemMeas = stg ^. stageBackground . Img.texture . Tex.srcSize

      elemMeas' = LO.calcFillCropSize maxMeas elemMeas
      elemPos = LO.alignCenter maxMeas elemMeas'
  in stg & (stageBackground . Img.position .~ elemPos)
         . (stageBackground . Img.size .~ elemMeas')


setLeft :: Image -> Stage -> IO Stage
setLeft i stg =
  stg & return . layoutLeft . (stageLeft .~ i)

layoutLeft :: Stage -> Stage
layoutLeft stg =
  let maxMeas = fromIntegral <$> stg ^. stageSize
      halfMaxMeas = maxMeas & Vec._x //~ 2

      elemMeas = stg ^. stageLeft . Img.texture . Tex.srcSize
      elemMeas' = LO.calcFitSize halfMaxMeas elemMeas
      elemPos = LO.alignBottomCenter halfMaxMeas elemMeas'

  in stg & (stageLeft . Img.position .~ elemPos)
         . (stageLeft . Img.size     .~ elemMeas')

setRight :: Image -> Stage -> IO Stage
setRight i stg =
  stg & return . layoutRight . (stageRight .~ i)

layoutRight :: Stage -> Stage
layoutRight stg =
  let maxMeas = fromIntegral <$> stg ^. stageSize
      halfMaxMeas = maxMeas & Vec._x //~ 2

      elemMeas = stg ^. stageRight . Img.texture . Tex.srcSize
      elemMeas' = LO.calcFitSize halfMaxMeas elemMeas

      elemPos = LO.alignBottomCenter halfMaxMeas elemMeas'
      elemPos' = elemPos & Vec._x +~ (halfMaxMeas ^. Vec._x)

  in stg & (stageRight . Img.position .~ elemPos')
         . (stageRight . Img.size .~ elemMeas')

setCenter :: Image -> Stage -> IO Stage
setCenter i stg =
  stg & return
      . layoutCenter
      . (stageCenter .~ i)

layoutCenter :: Stage -> Stage
layoutCenter stg =
  let maxMeas = fromIntegral <$> stg ^. stageSize
      elemMeas = stg ^. stageCenter . Img.texture . Tex.srcSize
      elemMeas' = LO.calcFitSize maxMeas elemMeas
      elemPos = LO.alignBottomCenter maxMeas elemMeas'

  in stg & (stageCenter . Img.position .~ elemPos)
         . (stageCenter . Img.size .~ elemMeas')

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
