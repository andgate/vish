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
import qualified Linear.Vector as Vec

import Control.Lens
import Control.Monad

draw :: Stage -> IO ()
draw stg =
  let mb = stg ^. stageMsgBox
      is =
        [ stg ^. stageBackground
        , stg ^. stageLeft
        , stg ^. stageRight
        , stg ^. stageCenter
        , mb  ^. MsgBox.text
        ]
      stgS = stg ^. stageSize
  in Img.drawAll stgS is


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
  let msgBox = stg ^. stageMsgBox
  msgBox' <- MsgBox.setMessage "" msgBox
  stg & return . (stageMsgBox .~ msgBox')

setBackground :: Image -> Stage -> IO Stage
setBackground i stg =
  stg & return . layoutBackground . (stageBackground .~ i)

clearBackground :: Stage -> IO Stage
clearBackground stg = do
  Img.unload (stg ^. stageBackground)
  blankImg   <- Img.blank
  stg & return
      . (stageBackground .~ blankImg)

layoutBackground :: Stage -> Stage
layoutBackground stg =
  let cS = fromIntegral <$> stg ^. stageSize
      eS = stg ^. stageBackground . Img.texture . Tex.srcSize
      lo = LO.Layout LO.AlignCenterH LO.AlignCenterV LO.CropFill
      (eP', eS') = LO.layout lo Vec.zero cS eS
  in stg & (stageBackground . Img.position .~ eP')
         . (stageBackground . Img.size .~ eS')


setLeft :: Image -> Stage -> IO Stage
setLeft i stg =
  stg & return . layoutLeft . (stageLeft .~ i)

layoutLeft :: Stage -> Stage
layoutLeft stg =
  let cS = fromIntegral <$> stg ^. stageSize
      eS = stg ^. stageLeft . Img.texture . Tex.srcSize
      lo = LO.Layout LO.AlignLeft LO.AlignBottom LO.Fit
      (eP', eS') = LO.layout lo Vec.zero cS eS
  in stg & (stageLeft . Img.position .~ eP')
         . (stageLeft . Img.size     .~ eS')


setRight :: Image -> Stage -> IO Stage
setRight i stg =
  stg & return . layoutRight . (stageRight .~ i)

layoutRight :: Stage -> Stage
layoutRight stg =
  let sS = fromIntegral <$> stg ^. stageSize
      cS = sS & Vec._x //~ 2
      cP = cS & Vec._y .~ 0
      eS = stg ^. stageRight . Img.texture . Tex.srcSize
      lo = LO.Layout LO.AlignRight LO.AlignBottom LO.Fit
      (eP', eS') = LO.layout lo cP cS eS
  in stg & (stageRight . Img.position .~ eP')
         . (stageRight . Img.size .~ eS')


setCenter :: Image -> Stage -> IO Stage
setCenter i stg =
  stg & return
      . layoutCenter
      . (stageCenter .~ i)

layoutCenter :: Stage -> Stage
layoutCenter stg =
  let cS = fromIntegral <$> stg ^. stageSize
      eS = stg ^. stageCenter . Img.texture . Tex.srcSize
      lo = LO.Layout LO.AlignCenterH LO.AlignBottom LO.Fit
      (eP', eS') = LO.layout lo Vec.zero cS eS
  in stg & (stageCenter . Img.position .~ eP')
         . (stageCenter . Img.size .~ eS')


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
