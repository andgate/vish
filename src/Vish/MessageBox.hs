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
import qualified Vish.Graphics.Texture as Tex

import Linear.V2 (V2 (..))
import qualified Linear.V2 as Vec
import qualified Linear.Vector as Vec
import qualified Linear.Metric as Vec

import Control.Lens

layout :: V2 Int -> MessageBox -> IO MessageBox
layout sS =
  layoutText . layoutSkin . layoutCell sS

layoutCell :: V2 Int     -- ^ Stage size
           -> MessageBox -- ^ Message box to resize
           -> MessageBox -- ^ Resized message box
layoutCell sS mb =
  let cS = fromIntegral <$> sS
      eS = cS & Vec._y //~ 3
      eP = eS & Vec._x .~ 0

      lo = LO.Layout LO.AlignCenterH LO.AlignBottom LO.None
      (eP', eS') = LO.layout lo Vec.zero cS eS
      fS = sqrt $ (eS'^.Vec._x/40 * eS'^.Vec._y/4)
  in mb & (position .~ eP')
        . (size     .~ eS')
        . (fontStyle . Font.pixelSize .~ (realToFrac fS))


setMessage :: String -> MessageBox -> IO MessageBox
setMessage str =
  layoutText . (content .~ str)

layoutText :: MessageBox -> IO MessageBox
layoutText mb = do
  mb' <- buildTextImage mb
  let cP = mb' ^. position
      cS = mb' ^. size
      eS = mb' ^. text . Img.texture . Tex.srcSize
      lo = LO.Layout LO.AlignCenterH LO.AlignTop LO.None
      (eP', eS') = LO.layout lo cP cS eS
  mb' & return
      . (text . Img.position .~ eP')
      . (text . Img.size     .~ eS')

buildTextImage :: MessageBox -> IO MessageBox
buildTextImage mb = do
  let sty = mb^.fontStyle
      str = mb^.content
  Img.unload (mb^.text)
  i <- Font.printToImage sty str
  mb & return
     . (text .~ i)

setSkin :: ImageAtlas -> MessageBox -> MessageBox
setSkin ia =
  layoutSkin . (skin .~ ia)

layoutSkin :: MessageBox -> MessageBox
layoutSkin mb =
  mb
