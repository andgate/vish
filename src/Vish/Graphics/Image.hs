module Vish.Graphics.Image
  ( module Vish.Graphics.Image
  , module Vish.Graphics.Data.Image
  )
where

import Vish.Graphics.Data.Image

import Vish.Graphics.Texture (Texture (..))
import qualified Vish.Graphics.Texture as Tex
import qualified Vish.Graphics.Util as Util

import Control.Monad

import Linear.V2 (V2 (..))
import qualified Linear.V2 as Vec
import qualified Linear.Vector as Vec

mkImage :: Texture -> Image
mkImage tex =
  let pos = Vec.zero
  in mkImageXY tex pos

mkImageXY :: Texture -> V2 Float -> Image
mkImageXY tex pos =
  let size = texSize tex
  in mkImageXYWH tex pos size

mkImageXYWH :: Texture -> V2 Float -> V2 Float -> Image
mkImageXYWH tex pos size=
  Image
    { imageTexture = tex
    , imagePosition = pos
    , imageSize = size
    }

drawAll :: V2 Int -> [Image] -> IO ()
drawAll scrnSize imgs =
  Util.withModelview scrnSize $
    forM_ imgs drawInView


draw :: V2 Int -> Image -> IO ()
draw scrnSize img =
  Util.withModelview scrnSize $
    drawInView img

drawInView :: Image -> IO ()
drawInView img =
  case img of
    Blank -> return ()
    Image tex pos size ->
      Tex.drawTexXYWH tex pos size


delete :: Image -> IO Image
delete Blank =
  return Blank
delete img = do
  Tex.unloadTexture . imageTexture $ img
  return Blank
