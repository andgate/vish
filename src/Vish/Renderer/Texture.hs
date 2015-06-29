module Vish.Renderer.Texture where

import qualified Codec.Picture as JP
import qualified Codec.Picture.Types as JP
import qualified Data.HashTable.IO as H
import qualified Data.Vector.Storable as V
import Vish.Renderer.Util
import Vish.Renderer.Data.Texture
import qualified Graphics.Rendering.OpenGL.GL as GL
import Graphics.Rendering.OpenGL.GL (($=))

fetchTexture :: TexCache -> String -> IO Texture
fetchTexture texCache path = do
  maybeTex <- H.lookup texCache path
  case maybeTex of
    Nothing -> loadTexture texCache path
    Just tex -> return tex

loadTexture :: TexCache -> String -> IO Texture
loadTexture texCache path = do
  eitherImage <- JP.readImage path
  case eitherImage of
    Left e -> error $ path ++ " unable to load with error: " ++ e
    Right img -> do
      tex <- texFromJPImg img
      H.insert texCache path tex
      return tex

texFromJPImg :: JP.DynamicImage -> IO Texture
texFromJPImg dImg =
  case dImg of
    JP.ImageY8 img -> error "Image format Y8 not supported"
    JP.ImageY16 img -> error "Image format Y16 not supported"
    JP.ImageYF img -> error "Image format YF not supported"
    JP.ImageYA8 img -> error "Image format YA8 not supported"
    JP.ImageYA16 img -> error "Image format YA16 not supported"
    JP.ImageRGB8 img -> uploadTexture img GL.RGB8 GL.RGB GL.UnsignedByte
    JP.ImageRGB16 img -> uploadTexture img GL.RGB16 GL.RGB GL.UnsignedShort
    JP.ImageRGBF img -> error "Image format RGBF not supported"
    JP.ImageRGBA8 img -> uploadTexture img GL.RGBA8 GL.RGBA GL.UnsignedByte
    JP.ImageRGBA16 img -> uploadTexture img GL.RGBA16 GL.RGBA GL.UnsignedShort
    JP.ImageYCbCr8 img -> texFromJPImg . JP.ImageRGB8 $ (JP.convertImage img :: (JP.Image JP.PixelRGB8))
    JP.ImageCMYK8 img -> error "Image format CMYK8 not supported"
    JP.ImageCMYK16 img -> error "Image format CMYK16 not supported"

uploadTexture :: (JP.Pixel p) => JP.Image p -> GL.PixelInternalFormat -> GL.PixelFormat -> GL.DataType -> IO Texture
uploadTexture (JP.Image w h dat) pixelInternalFormat pixelFormat datatype = do
  [tex] <- GL.genObjectNames 1
  GL.textureBinding GL.Texture2D $= Just tex

  V.unsafeWith dat $ \ptr ->
    GL.texImage2D
      GL.Texture2D
      GL.NoProxy
      0
      pixelInternalFormat
      (GL.TextureSize2D (gsizei w) (gsizei h))
      0
      (GL.PixelData pixelFormat datatype ptr)

  return Texture
    { texWidth = w
    , texHeight = h
    , texObject = tex
    }
