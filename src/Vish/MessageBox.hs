module Vish.MessageBox
  ( module Vish.MessageBox
  , module Vish.Data.MessageBox
  )
where

import Vish.Data.MessageBox

import qualified Vish.Layout as LO

import qualified Vish.Graphics.Image as Img
import Vish.Graphics.ImageAtlas (ImageAtlas)
import qualified Vish.Graphics.ImageAtlas as ImgAtlas
import qualified Vish.Graphics.Font as Font

import Linear.V2 (V2 (..))
import qualified Linear.V2 as Vec

import Control.Lens

layout :: V2 Int -> MessageBox -> IO MessageBox
layout sS =
  layoutText . layoutSkin . layoutCell sS

layoutCell :: V2 Int     -- ^ Stage size
           -> MessageBox -- ^ Message box to resize
           -> MessageBox -- ^ Resized message box
layoutCell sS mb =
  let sS' = fromIntegral <$> sS
      c = mb^.content
      mbS = sS' & Vec._y //~ 3
      fS = (mbS^.Vec._y) / 3
      mbP = LO.alignBottomCenter sS' mbS
  in mb & (size .~ mbS)
        . (position .~ mbP)
        . (fontStyle . Font.pixelSize .~ fS)

setSkin :: ImageAtlas -> MessageBox -> MessageBox
setSkin ia mb = undefined

layoutSkin :: MessageBox -> MessageBox
layoutSkin mb = mb

setMessage :: String -> MessageBox -> IO MessageBox
setMessage str =
  layoutText . (content .~ str)

layoutText :: MessageBox -> IO MessageBox
layoutText mb = do
  let sty = mb^.fontStyle
      str = mb^.content

  Img.unload (mb ^. text)
  i <- Font.printToImage sty str

  let s = i ^. Img.size
      p = LO.alignCenter (mb ^. size) s
      p' = (mb ^. position) + p
      i' = i & Img.position .~ p'
  mb & return . (text .~ i')
