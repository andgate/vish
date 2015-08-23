module Vish.MessageBox
  ( module Vish.MessageBox
  , module Vish.Data.MessageBox
  )
where

import Vish.Data.MessageBox

import qualified Vish.MessageBoxSkin as Skin
import qualified Vish.Layout as LO

import qualified Vish.Graphics.Image as Img
import Vish.Graphics.ImageAtlas (ImageAtlas)
import qualified Vish.Graphics.Font as Font
import qualified Vish.Graphics.Texture as Tex

import Linear.V2 (V2 (..))
import qualified Linear.V2 as Vec2
import qualified Linear.Vector as Vec

import Control.Lens

layout :: V2 Int -> MessageBox -> IO MessageBox
layout sS =
  layoutText . layoutSkin . layoutCell sS

layoutCell :: V2 Int     -- ^ Stage size
           -> MessageBox -- ^ Message box to resize
           -> MessageBox -- ^ Resized message box
layoutCell sS mb =
  let cS = fromIntegral <$> sS

      eS = cS & (Vec2._x *~ 6/8)
              . (Vec2._y //~ 3)
      eP = eS & Vec2._x .~ 0

      lo = LO.Layout LO.AlignCenterH LO.AlignBottom LO.None
      (eP', eS') = LO.layout lo Vec.zero cS eS
      fS = sqrt $ (eS'^.Vec2._x/40 * eS'^.Vec2._y/4)
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
      lo = LO.Layout LO.AlignCenterH LO.AlignCenterV LO.None
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
  layoutSkin . (skin .~ (Skin.fromAtlas ia))

layoutSkin :: MessageBox -> MessageBox
layoutSkin =
  layoutSkinBottomRight
  . layoutSkinBottom
  . layoutSkinBottomLeft
  . layoutSkinRight
  . layoutSkinCenter
  . layoutSkinLeft
  . layoutSkinTopRight
  . layoutSkinTop
  . layoutSkinTopLeft

layoutSkinTopLeft :: MessageBox -> MessageBox
layoutSkinTopLeft mb =
  mb & (skin . Skin.topLeft . Img.position .~ eP')
     . (skin . Skin.topLeft . Img.size     .~ eS')
  where
    cP = mb ^. position
    cS = mb ^. size
    eS = mb ^. skin . Skin.topLeft . Img.texture . Tex.srcSize
    lo = LO.Layout LO.AlignLeft LO.AlignTop LO.None
    (eP', eS') = LO.layout lo cP cS eS

layoutSkinTop :: MessageBox -> MessageBox
layoutSkinTop mb =
  mb & (skin . Skin.top . Img.position .~ eP')
     . (skin . Skin.top . Img.size     .~ eS')
  where
    tlW = mb ^. skin . Skin.topLeft  . Img.size . Vec2._x
    trW = mb ^. skin . Skin.topRight . Img.size . Vec2._x

    mbP = mb ^. position
    cP = mbP & Vec2._x +~ tlW

    mbS = mb ^. size
    cS = mbS & Vec2._x -~ (tlW + trW)

    eS = mb ^. skin . Skin.top . Img.texture . Tex.srcSize
    lo = LO.Layout LO.AlignCenterH LO.AlignTop LO.StretchX
    (eP', eS') = LO.layout lo cP cS eS

layoutSkinTopRight :: MessageBox -> MessageBox
layoutSkinTopRight mb =
  mb & (skin . Skin.topRight . Img.position .~ eP')
     . (skin . Skin.topRight . Img.size     .~ eS')
  where
    cP = mb ^. position
    cS = mb ^. size

    eS = mb ^. skin . Skin.topRight . Img.texture . Tex.srcSize
    lo = LO.Layout LO.AlignRight LO.AlignTop LO.None
    (eP', eS') = LO.layout lo cP cS eS

layoutSkinLeft :: MessageBox -> MessageBox
layoutSkinLeft mb =
  mb & (skin . Skin.left . Img.position .~ eP')
     . (skin . Skin.left . Img.size     .~ eS')
  where
    tlH = mb ^. skin . Skin.topLeft    . Img.size . Vec2._y
    blH = mb ^. skin . Skin.bottomLeft . Img.size . Vec2._y

    mbP = mb ^. position
    cP = mbP & Vec2._y +~ tlH

    mbS = mb ^. size
    cS = mbS & Vec2._y -~ (tlH + blH)

    eS = mb ^. skin . Skin.left . Img.texture . Tex.srcSize
    lo = LO.Layout LO.AlignLeft LO.AlignCenterV LO.StretchY
    (eP', eS') = LO.layout lo cP cS eS

layoutSkinCenter :: MessageBox -> MessageBox
layoutSkinCenter mb =
  mb & (skin . Skin.center . Img.position .~ eP')
     . (skin . Skin.center . Img.size     .~ eS')
  where
    rW = mb ^. skin . Skin.right  . Img.size . Vec2._x
    lW = mb ^. skin . Skin.left   . Img.size . Vec2._x
    tH = mb ^. skin . Skin.top    . Img.size . Vec2._y
    bH = mb ^. skin . Skin.bottom . Img.size . Vec2._y

    mbP = mb ^. position
    cP = mbP & (Vec2._x +~ lW)
             . (Vec2._y +~ tH)


    mbS = mb ^. size
    cS = mbS & (Vec2._x -~ (lW + rW))
             . (Vec2._y -~ (tH + bH))

    eS = mb ^. skin . Skin.center . Img.texture . Tex.srcSize
    lo = LO.Layout LO.AlignCenterH LO.AlignCenterV LO.Stretch
    (eP', eS') = LO.layout lo cP cS eS

layoutSkinRight :: MessageBox -> MessageBox
layoutSkinRight mb =
  mb & (skin . Skin.right . Img.position .~ eP')
     . (skin . Skin.right . Img.size     .~ eS')
  where
    trH = mb ^. skin . Skin.topRight    . Img.size . Vec2._y
    brH = mb ^. skin . Skin.bottomRight . Img.size . Vec2._y

    mbP = mb ^. position
    cP = mbP & Vec2._y +~ trH

    mbS = mb ^. size
    cS = mbS & Vec2._y -~ (trH + brH)

    eS = mb ^. skin . Skin.right . Img.texture . Tex.srcSize
    lo = LO.Layout LO.AlignRight LO.AlignCenterV LO.StretchY
    (eP', eS') = LO.layout lo cP cS eS

layoutSkinBottomLeft :: MessageBox -> MessageBox
layoutSkinBottomLeft mb =
  mb & (skin . Skin.bottomLeft . Img.position .~ eP')
     . (skin . Skin.bottomLeft . Img.size     .~ eS')
  where
    cP = mb ^. position
    cS = mb ^. size

    eS = mb ^. skin . Skin.bottomLeft . Img.texture . Tex.srcSize
    lo = LO.Layout LO.AlignLeft LO.AlignBottom LO.None
    (eP', eS') = LO.layout lo cP cS eS

layoutSkinBottom :: MessageBox -> MessageBox
layoutSkinBottom mb =
  mb & (skin . Skin.bottom . Img.position .~ eP')
     . (skin . Skin.bottom . Img.size     .~ eS')
  where
    blW = mb ^. skin . Skin.bottomLeft  . Img.size . Vec2._x
    brW = mb ^. skin . Skin.bottomRight . Img.size . Vec2._x

    mbP = mb ^. position
    cP = mbP & Vec2._x +~ blW

    mbS = mb ^. size
    cS = mbS & Vec2._x -~ (blW + brW)

    eS = mb ^. skin . Skin.bottomLeft . Img.texture . Tex.srcSize
    lo = LO.Layout LO.AlignCenterH LO.AlignBottom LO.StretchX
    (eP', eS') = LO.layout lo cP cS eS

layoutSkinBottomRight :: MessageBox -> MessageBox
layoutSkinBottomRight mb =
  mb & (skin . Skin.bottomRight . Img.position .~ eP')
     . (skin . Skin.bottomRight . Img.size     .~ eS')
  where
    cP = mb ^. position
    cS = mb ^. size

    eS = mb ^. skin . Skin.bottomRight . Img.texture . Tex.srcSize
    lo = LO.Layout LO.AlignRight LO.AlignBottom LO.None
    (eP', eS') = LO.layout lo cP cS eS
