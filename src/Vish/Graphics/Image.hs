module Vish.Graphics.Image
  ( module Vish.Graphics.Image
  , module Vish.Graphics.Data.Image
  )
where

import Vish.Graphics.Data.Image

import Vish.Graphics.Texture (Texture)
import qualified Vish.Graphics.Texture as Tex
import qualified Vish.Graphics.Util as Util

import Control.Lens
import Control.Monad

import Linear.V2 (V2 (..))
import qualified Linear.Vector as Vec


drawAll :: V2 Int  -- ^ Screen size
        -> [Image] -- ^ Images to draw, in-order.
        -> IO ()
drawAll scrnSize imgs =
  Util.withModelview scrnSize $
    forM_ imgs drawInView

draw :: V2 Int -- ^ Screen size
     -> Image  -- ^ Image to draw
     -> IO ()
draw scrnSize img =
  Util.withModelview scrnSize $
    drawInView img

drawInView :: Image -> IO ()
drawInView (Image tex pos meas) =
  Tex.drawTexXYWH tex pos meas


load :: FilePath -- ^ Image file path
     -> IO Image
load fp =
  liftM mkImage $ Tex.load fp

loadXY :: V2 Float -- ^ Image position
       -> FilePath -- ^ Image file path
       -> IO Image
loadXY p fp =
  liftM (mkImageXY p) $ Tex.load fp

loadXYWH :: V2 Float -- ^ Image position
         -> V2 Float -- ^ Image size
         -> FilePath -- ^ Image file path
         -> IO Image
loadXYWH p s fp =
  liftM (mkImageXYWH p s) $ Tex.load fp

unload :: Image -> IO ()
unload i =
  Tex.unload (i^.texture)

mkImage :: Texture -- ^ Image texture
        -> Image
mkImage t =
  let p = Vec.zero
  in mkImageXY p t

mkImageXY :: V2 Float -- ^ Image position
          -> Texture  -- ^ Image texture
          -> Image
mkImageXY p t =
  mkImageXYWH p (t^.Tex.srcSize) t

mkImageXYWH :: V2 Float -- ^ Image position
            -> V2 Float -- ^ Image measurements
            -> Texture  -- ^ Image texture
            -> Image
mkImageXYWH p s t =
  Image
    { _texture  = t
    , _position = p
    , _size  = s
    }
