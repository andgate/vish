module Vish.Graphics.Image
  ( module Vish.Graphics.Image
  , module Vish.Graphics.Data.Image
  )
where

import Vish.Graphics.Data.Image
import Vish.Graphics.Texture
import Vish.Graphics.Util

import Vish.Math.Vector (Vector2f (..))
import qualified Vish.Math.Vector as Vec

mkImage :: Texture -> Image
mkImage tex =
  let pos = Vec.zero
  in mkImageXY tex pos

mkImageXY :: Texture -> Vector2f -> Image
mkImageXY tex pos =
  let size = texSize tex
  in mkImageXYWH tex pos size

mkImageXYWH :: Texture -> Vector2f -> Vector2f -> Image
mkImageXYWH tex pos size=
  Image
    { imageTexture = tex
    , imagePosition = pos
    , imageSize = size
    }

drawImages :: (Int, Int) -> [Image] -> IO ()
drawImages screenSize imgs =
  withModelview screenSize $
    mapM_ drawImage imgs

drawImage :: Image -> IO ()
drawImage Blank =
  return ()
drawImage img =
  let tex = imageTexture img
      pos = imagePosition img
      size = imageSize img
  in drawTexXYWH tex pos size
