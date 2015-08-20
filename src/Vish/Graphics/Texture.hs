module Vish.Graphics.Texture
  ( module Vish.Graphics.Texture
  , module Vish.Graphics.Data.Texture
  )
where

import Vish.Graphics.Util
import Vish.Graphics.Data.Texture

import Control.Monad
import qualified Codec.Picture as JP
import qualified Codec.Picture.Types as JP
import qualified Data.HashTable.IO as H
import qualified Data.Vector.Storable as V

import Linear.V2 (V2 (..))
import qualified Linear.Vector as Vec

import qualified Graphics.Rendering.OpenGL.GL as GL
import Graphics.Rendering.OpenGL.GL (($=), get)

supportedExtensions :: [String]
supportedExtensions = ["bmp", "jpg", "png", "tga", "tiff"]

mkTexCache :: IO TexCache
mkTexCache = H.new

installTexture :: TexCache -> FilePath -> String -> IO ()
installTexture texCache path tag =
  loadTexture path >>= either error (cacheTexture texCache tag)

uncacheTexture :: TexCache -> String -> IO ()
uncacheTexture = H.delete

uninstallTexture :: TexCache -> String -> IO ()
uninstallTexture texCache tag =
  fetchTexture texCache tag >>= either putStrLn (unsafeUninstallTexture texCache tag)

unsafeUninstallTexture :: TexCache -> String -> Texture -> IO ()
unsafeUninstallTexture texCache tag tex =
  uncacheTexture texCache tag >> unloadTexture tex

-- | Uninstall all the textures and empty the
-- entries in the texture cache.
scrubTexCache :: TexCache -> IO ()
scrubTexCache texCache =
  H.toList texCache >>= mapM_ (uncurry $ unsafeUninstallTexture texCache)

cacheTexture :: TexCache -> String -> Texture -> IO ()
cacheTexture texCache tag tex =
  H.insert texCache tag tex

fetchTexture :: TexCache -> String -> IO (Either String Texture)
fetchTexture texCache path =
  liftM (maybe (Left noTexMsg) Right) $ H.lookup texCache path
  where noTexMsg = "Texture not cached at " ++ path

drawTex :: Texture -> IO ()
drawTex tex =
  drawTexXY tex Vec.zero

drawTexXY :: Texture -> V2 Float -> IO ()
drawTexXY tex pos =
  let texSize = textureSize tex
  in drawTexXYWH tex pos texSize

drawTexXYWH :: Texture -> V2 Float -> V2 Float -> IO ()
drawTexXYWH tex (V2 x y) (V2 w h) = do
  --print $ "Drawing " ++ texPath tex ++ " " ++ show (x,y,w,h)

  -- Set up wrap and filtering mode
  GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.Repeat)
  GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.Repeat)
  GL.textureFilter   GL.Texture2D      $= ((GL.Nearest, Nothing), GL.Nearest)

  -- Enable texturing
  GL.texture GL.Texture2D $= GL.Enabled
  GL.textureFunction      $= GL.Combine

  -- Set current texture
  GL.textureBinding GL.Texture2D $= Just (textureObject tex)

  oldColor <- get GL.currentColor
  GL.currentColor $= GL.Color4 1.0 1.0 1.0 1.0

  GL.renderPrimitive GL.Quads $
    zipWithM_
      (\(vX, vY) (tX, tY) -> do
        GL.texCoord $ GL.TexCoord2 (gf tX) (gf tY)
        GL.vertex   $ GL.Vertex2   (gf vX) (gf vY))
      imagePath
      [(0.0, 0.0), (1.0, 0.0), (1.0, 1.0), (0.0, 1.0)]

  GL.currentColor $= oldColor

  GL.texture GL.Texture2D $= GL.Disabled

  where
    imagePath :: [(Float, Float)]
    imagePath =
      [(x, y), (x+w, y), (x+w, y+h), (x,y+h)]

unloadTexture :: Texture -> IO ()
unloadTexture tex = GL.deleteObjectName $ textureObject tex

loadTexture :: FilePath -> IO (Either String Texture)
loadTexture path =
  JP.readImage path >>= either (return . Left) (texFromJPImg path)

texFromJPImg :: FilePath -> JP.DynamicImage -> IO (Either String Texture)
texFromJPImg path dImg =
  case dImg of
    JP.ImageY8 img -> Right `liftM` gpuLoadTexture path img GL.Luminance8 GL.Luminance GL.UnsignedByte
    JP.ImageY16 img -> Right `liftM` gpuLoadTexture path img GL.Luminance16 GL.Luminance GL.UnsignedShort
    JP.ImageYF _ -> return $ Left "32bit Greyscale is unsupported."
    JP.ImageYA8 img -> Right `liftM` gpuLoadTexture path img GL.Luminance8Alpha8 GL.LuminanceAlpha GL.UnsignedByte
    JP.ImageYA16 img -> Right `liftM` gpuLoadTexture path img GL.Luminance16Alpha16 GL.LuminanceAlpha GL.UnsignedShort
    JP.ImageRGB8 img -> Right `liftM` gpuLoadTexture path img GL.RGB8 GL.RGB GL.UnsignedByte
    JP.ImageRGB16 img -> Right `liftM` gpuLoadTexture path img GL.RGB16 GL.RGB GL.UnsignedShort
    JP.ImageRGBF _ -> return $ Left "32bit RGB is unsupported"
    JP.ImageRGBA8 img -> Right `liftM` gpuLoadTexture path img GL.RGBA8 GL.RGBA GL.UnsignedByte
    JP.ImageRGBA16 img -> Right `liftM` gpuLoadTexture path img GL.RGBA16 GL.RGBA GL.UnsignedShort
    JP.ImageYCbCr8 img -> texFromJPImg path . JP.ImageRGB8 $ (JP.convertImage img :: (JP.Image JP.PixelRGB8))
    JP.ImageCMYK8 img -> texFromJPImg path . JP.ImageRGB8 $ (JP.convertImage img :: (JP.Image JP.PixelRGB8))
    JP.ImageCMYK16 img -> texFromJPImg path . JP.ImageRGB16 $ (JP.convertImage img :: (JP.Image JP.PixelRGB16))

gpuLoadTexture :: (JP.Pixel p) => FilePath -> JP.Image p -> GL.PixelInternalFormat -> GL.PixelFormat -> GL.DataType -> IO Texture
gpuLoadTexture path (JP.Image w h dat) pixelInternalFormat pixelFormat datatype = do
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
    { texturePath = path
    , textureSize =  fromIntegral <$> V2 w h
    , textureObject = tex
    }
