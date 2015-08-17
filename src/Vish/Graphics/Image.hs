module Vish.Graphics.Image
  ( module Vish.Graphics.Image
  , module Vish.Graphics.Data.Image
  )
where

import Vish.Graphics.Data.Image
import Vish.Graphics.Texture
import Vish.Graphics.Util

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
drawAll screenSize imgs =
  withModelview screenSize $
    mapM_ draw imgs

draw :: Image -> IO ()
draw Blank =
  return ()
draw img =
  let tex = imageTexture img
      pos = imagePosition img
      size = imageSize img
  in drawTexXYWH tex pos size
