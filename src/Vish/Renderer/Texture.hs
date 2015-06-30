module Vish.Renderer.Texture where


import Vish.Renderer.Util
import Vish.Renderer.Data.Texture

import Control.Monad
import qualified Codec.Picture as JP
import qualified Codec.Picture.Types as JP
import qualified Data.HashTable.IO as H
import qualified Data.Vector.Storable as V
import qualified Graphics.Rendering.OpenGL.GL as GL
import Graphics.Rendering.OpenGL.GL (($=), get)

drawTexture :: Texture -> IO ()
drawTexture tex = do
  let Texture w h _ = tex
  -- Set up wrap and filtering mode
  GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.Repeat)
  GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.Repeat)
  GL.textureFilter   GL.Texture2D      $= ((GL.Nearest, Nothing), GL.Nearest)

  -- Enable texturing
  GL.texture GL.Texture2D $= GL.Enabled
  GL.textureFunction      $= GL.Combine

  -- Set current texture
  GL.textureBinding GL.Texture2D $= Just (texObject tex)

  oldColor <- get GL.currentColor
  GL.currentColor $= GL.Color4 1.0 1.0 1.0 1.0

  GL.renderPrimitive GL.Quads $
    zipWithM_
      (\(vX, vY) (tX, tY) -> do
        GL.texCoord $ GL.TexCoord2 (gf tX) (gf tY)
        GL.vertex   $ GL.Vertex2   (gf vX) (gf vY))
      (imagePath (fromIntegral w) (fromIntegral h))
      [(0, 0), (1.0, 0), (1.0, 1.0), (0, 1.0)]

  GL.currentColor $= oldColor

  GL.texture GL.Texture2D $= GL.Disabled

  where
    imagePath :: Float -> Float -> [(Float, Float)]
    imagePath w h =
      [(0, 0), (w, 0), (w, h), (0,h)]

fetchTexture :: TexCache -> String -> IO (Either String Texture)
fetchTexture texCache path = do
  maybeTex <- H.lookup texCache path
  case maybeTex of
    Nothing -> loadTexture texCache path
    Just tex -> return $ Right tex

unloadTexture :: TexCache -> String -> IO ()
unloadTexture texCache path = do
  maybeTex <- H.lookup texCache path
  case maybeTex of
    Nothing -> return ()
    Just tex -> do
      uninstallTexture tex
      H.delete texCache path


loadTexture :: TexCache -> String -> IO (Either String Texture)
loadTexture texCache path = do
  eitherImage <- JP.readImage path
  case eitherImage of
    Left e -> do
      let msg = path ++ " unable to load with error: " ++ e
      putStrLn msg
      return $ Left msg
    Right img -> do
      tex <- texFromJPImg img
      H.insert texCache path tex
      return $ Right tex

texFromJPImg :: JP.DynamicImage -> IO Texture
texFromJPImg dImg =
  case dImg of
    JP.ImageY8 img -> error "Image format Y8 not supported"
    JP.ImageY16 img -> error "Image format Y16 not supported"
    JP.ImageYF img -> error "Image format YF not supported"
    JP.ImageYA8 img -> error "Image format YA8 not supported"
    JP.ImageYA16 img -> error "Image format YA16 not supported"
    JP.ImageRGB8 img -> installTexture img GL.RGB8 GL.RGB GL.UnsignedByte
    JP.ImageRGB16 img -> installTexture img GL.RGB16 GL.RGB GL.UnsignedShort
    JP.ImageRGBF img -> error "Image format RGBF not supported"
    JP.ImageRGBA8 img -> installTexture img GL.RGBA8 GL.RGBA GL.UnsignedByte
    JP.ImageRGBA16 img -> installTexture img GL.RGBA16 GL.RGBA GL.UnsignedShort
    JP.ImageYCbCr8 img -> texFromJPImg . JP.ImageRGB8 $ (JP.convertImage img :: (JP.Image JP.PixelRGB8))
    JP.ImageCMYK8 img -> error "Image format CMYK8 not supported"
    JP.ImageCMYK16 img -> error "Image format CMYK16 not supported"

installTexture :: (JP.Pixel p) => JP.Image p -> GL.PixelInternalFormat -> GL.PixelFormat -> GL.DataType -> IO Texture
installTexture (JP.Image w h dat) pixelInternalFormat pixelFormat datatype = do
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

uninstallTexture :: Texture -> IO ()
uninstallTexture tex = GL.deleteObjectName $ texObject tex
