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
setMessage str mb = do
  let sty = mb ^. msgBoxFontStyle
  Img.unload (mb ^. msgBoxImg)
  i <- Font.printToImage sty str

  let s = i ^. Img.size
      p = LO.alignCenter (mb ^. msgBoxSize) s
      p' = (mb ^. msgBoxPosition) + p
      i' = i & Img.position .~ p'
  mb & return . (msgBoxImg .~ i')
                  . (msgBoxContent .~ str)
