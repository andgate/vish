module Vish.Graphics.Font
  ( module Vish.Graphics.Font
  , module Vish.Graphics.Data.Font
  , BoundingBox (..)
  , Font
  )
where

import Vish.Graphics.Data.Font

import qualified Vish.Graphics.Data.Color as C

import Vish.Graphics.Image (Image (..))
import qualified Vish.Graphics.Image as Img

import Vish.Graphics.Texture (Texture (..))
import qualified Vish.Graphics.Texture as Tex

import Vish.Graphics.Util

import Control.Monad

import Codec.Picture( PixelRGBA8( .. ) )

import Graphics.Text.TrueType (Font, BoundingBox (..))
import qualified Graphics.Text.TrueType as Font

import qualified Graphics.Rasterific as R
import qualified Graphics.Rasterific.Texture as R

import qualified Graphics.Rendering.OpenGL.GL as GL
import Graphics.Rendering.OpenGL.GL (($=), get)

import Linear.V2 (V2 (..))
import qualified Linear.V2 as Vec
import qualified Linear.Vector as Vec

import System.FilePath

load :: FilePath -> IO Font
load = liftM (either error id) . Font.loadFontFile

printToImage :: Style -> String -> IO Image
printToImage style str =
  printToImageXY style str Vec.zero

-- Needs to load the vertices to the
-- for drawing later
printToImageXY :: Style -> String -> V2 Float -> IO Image
printToImageXY (Style fnt (C.Color r g b a) spx)
             str
             (V2 x y)
  = do
  let dpi = 200
      spt = Font.pixelSizeInPointAtDpi spx dpi
      (BoundingBox x1 y1 x2 y2 blH) =
        Font.stringBoundingBox fnt dpi spt str
      imgW = ceiling $ x2 - x1
      imgH = ceiling $ y2 - y1
      jpImg =
        R.renderDrawingAtDpi imgW imgH dpi (PixelRGBA8 255 255 255 0)
          . R.withTexture (R.uniformTexture $ PixelRGBA8 r g b a) $
            R.printTextAt fnt spt (R.V2 0 y2) str
  tex <- Tex.gpuLoadTexture "" jpImg GL.RGBA8 GL.RGBA GL.UnsignedByte
  return $ Img.mkImageXY tex (V2 x y)
