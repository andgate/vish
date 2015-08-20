module Vish.MessageBox
  ( module Vish.MessageBox
  , module Vish.Data.MessageBox
  )
where

import Vish.Data.MessageBox

import qualified Vish.Layout as LO

import qualified Vish.Graphics.Image as Img
import qualified Vish.Graphics.Font as Font

import Linear.V2 (V2 (..))
import qualified Linear.V2 as Vec

import Control.Lens

resize :: V2 Int -> MessageBox -> IO MessageBox
resize stgSize msgBox = do
  let stgSizeF = fromIntegral <$> stgSize
      content = msgBox ^. msgBoxContent
      size = stgSizeF & Vec._y //~ 3
      fontSize = (size ^. Vec._y) / 3
      pos = LO.alignBottomCenter stgSizeF size
      msgBox' = msgBox & (msgBoxSize .~ size)
                       . (msgBoxPosition .~ pos)
                       . (msgBoxFontStyle . Font.pixelSize .~ fontSize)
  setMessage content msgBox'


setMessage :: String -> MessageBox -> IO MessageBox
setMessage [] msgBox =
   msgBox & return . (msgBoxContent .~ []) . (msgBoxImg .~ Img.Blank)
setMessage str msgBox = do
  let style = msgBox ^. msgBoxFontStyle
  img <- Font.printToImage style str

  let msgSize = Img.imageSize img
      msgBoxCenterPos = LO.alignCenter (msgBox ^. msgBoxSize) msgSize
      pos = (msgBox ^. msgBoxPosition) + msgBoxCenterPos
  msgBox & return . (msgBoxImg .~ img {Img.imagePosition = pos})
                  . (msgBoxContent .~ str)
