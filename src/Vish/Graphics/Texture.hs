module Vish.Graphics.Texture
  ( module Vish.Graphics.Texture
  , module Vish.Graphics.Data.Texture
  )
where

import Vish.Graphics.Data.Texture
import Vish.Graphics.Util

import Control.Lens
import Control.Monad
import Data.IORef

import qualified Codec.Picture as JP
import qualified Codec.Picture.Types as JP
import qualified Data.Vector.Storable as V

import Linear.V2 (V2 (..))
import qualified Linear.Vector as Vec

import qualified Graphics.Rendering.OpenGL.GL as GL
import Graphics.Rendering.OpenGL.GL (($=), get)

supportedExtensions :: [String]
supportedExtensions = ["bmp", "jpg", "png", "tga", "tiff"]

drawTex :: Texture -- ^ The texture to draw
        -> IO ()
drawTex tex =
  drawTexXY tex Vec.zero

drawTexXY :: Texture  -- ^ The texture to draw
          -> V2 Float -- ^ The position to draw at
          -> IO ()
drawTexXY t p =
  drawTexXYWH t p (t^.size)

drawTexXYWH :: Texture  -- ^ The texture to draw
            -> V2 Float -- ^ The position to draw at
            -> V2 Float -- ^ The size to draw as
            -> IO ()
drawTexXYWH t p s = do
  texObjMaybe <- readIORef (t^.object)
  case texObjMaybe of
    Nothing -> return ()
    Just _ -> do
      -- Set up wrap and filtering mode
      GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.Repeat)
      GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.Repeat)
      GL.textureFilter   GL.Texture2D      $= ((GL.Nearest, Nothing), GL.Nearest)

      -- Enable texturing
      GL.texture GL.Texture2D $= GL.Enabled
      GL.textureFunction      $= GL.Combine

      -- Set current texture
      GL.textureBinding GL.Texture2D $= texObjMaybe

      oldColor <- get GL.currentColor
      GL.currentColor $= GL.Color4 1.0 1.0 1.0 1.0



      GL.renderPrimitive GL.Quads $
        zipWithM_
          (\(vX, vY) (tX, tY) -> do
            GL.texCoord $ GL.TexCoord2 (gf tX) (gf tY)
            GL.vertex   $ GL.Vertex2   (gf vX) (gf vY))
          (rectPath p s)
          (rectPath (t^.position) (t^.size))

      GL.currentColor $= oldColor

      GL.texture GL.Texture2D $= GL.Disabled
  where
    rectPath :: V2 Float -> V2 Float -> [(Float, Float)]
    rectPath (V2 x y) (V2 w h) =
      [(x, y), (x+w, y), (x+w, y+h), (x,y+h)]


load :: FilePath -> IO Texture
load texPath =
  JP.readImage texPath
    >>= either error (fromJPImage texPath)
    >>= either error return


fromJPImage :: FilePath -> JP.DynamicImage -> IO (Either String Texture)
fromJPImage fp di =
  case di of
    JP.ImageY8     i -> Right `liftM` sendToGpu fp i GL.Luminance8  GL.Luminance GL.UnsignedByte
    JP.ImageY16    i -> Right `liftM` sendToGpu fp i GL.Luminance16 GL.Luminance GL.UnsignedShort
    JP.ImageYF     _ -> return $ Left "32bit Greyscale is unsupported."
    JP.ImageYA8    i -> Right `liftM` sendToGpu fp i GL.Luminance8Alpha8   GL.LuminanceAlpha GL.UnsignedByte
    JP.ImageYA16   i -> Right `liftM` sendToGpu fp i GL.Luminance16Alpha16 GL.LuminanceAlpha GL.UnsignedShort
    JP.ImageRGB8   i -> Right `liftM` sendToGpu fp i GL.RGB8  GL.RGB GL.UnsignedByte
    JP.ImageRGB16  i -> Right `liftM` sendToGpu fp i GL.RGB16 GL.RGB GL.UnsignedShort
    JP.ImageRGBF   _ -> return $ Left "32bit RGB is unsupported"
    JP.ImageRGBA8  i -> Right `liftM` sendToGpu fp i GL.RGBA8  GL.RGBA GL.UnsignedByte
    JP.ImageRGBA16 i -> Right `liftM` sendToGpu fp i GL.RGBA16 GL.RGBA GL.UnsignedShort
    JP.ImageYCbCr8 i -> fromJPImage fp . JP.ImageRGB8  $ (JP.convertImage i :: (JP.Image JP.PixelRGB8))
    JP.ImageCMYK8  i -> fromJPImage fp . JP.ImageRGB8  $ (JP.convertImage i :: (JP.Image JP.PixelRGB8))
    JP.ImageCMYK16 i -> fromJPImage fp . JP.ImageRGB16 $ (JP.convertImage i :: (JP.Image JP.PixelRGB16))


sendToGpu :: (JP.Pixel p) => FilePath -> JP.Image p -> GL.PixelInternalFormat -> GL.PixelFormat -> GL.DataType -> IO Texture
sendToGpu texPath (JP.Image w h dat) pixelInternalFormat pixelFormat datatype = do
  [texObjName] <- GL.genObjectNames 1
  GL.textureBinding GL.Texture2D $= Just texObjName
  GL.textureFilter   GL.Texture2D      $= ((GL.Nearest, Nothing), GL.Nearest)

  V.unsafeWith dat $ \ptr ->
    GL.texImage2D
      GL.Texture2D
      GL.NoProxy
      0
      pixelInternalFormat
      (GL.TextureSize2D (gsizei w) (gsizei h))
      0
      (GL.PixelData pixelFormat datatype ptr)

  texObj <- newIORef $ Just texObjName

  return Texture
    { _path     = texPath
    , _srcSize  =  fromIntegral <$> V2 w h
    , _position = Vec.zero
    , _size     = V2 1 1
    , _object   = texObj
    }


unload :: Texture -> IO ()
unload t = do
  let texObj = t^.object
  texObjMaybe <- readIORef texObj
  case texObjMaybe of
    Nothing -> return ()
    Just texObjName -> do
      GL.deleteObjectName texObjName
      writeIORef texObj Nothing
