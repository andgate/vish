module Vish.Graphics.ImageAtlas
  ( module Vish.Graphics.ImageAtlas
  , module Vish.Graphics.Data.ImageAtlas
  )
where

import Vish.Graphics.Data.ImageAtlas

import Vish.Graphics.Image (Image)
import qualified Vish.Graphics.Image as Img
import qualified Vish.Graphics.Texture as Tex

import Linear.V2 (V2 (..))
import Linear.V4 (V4 (..))
import qualified Linear.V2 as Vec2
import qualified Linear.V4 as Vec4

import Control.Lens
import Control.Monad
import Data.IORef
import Data.Maybe
import Data.Yaml

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import qualified Graphics.Rendering.OpenGL.GL as GL
import Graphics.Rendering.OpenGL.GL (($=), get)

import System.FilePath

blank :: ImageAtlas
blank =
  HM.empty

load :: FilePath -- ^ Image file path
     -> FilePath -- ^ Atlas file path
     -> IO ImageAtlas
load ifp afp = do
  i <- Img.load ifp
  rs <- loadAtlas afp
  return $ toImageAtlas i rs

toImageAtlas :: Image
             -> [AtlasRegion]
             -> ImageAtlas
toImageAtlas i rs =
  HM.fromList . map (toSubImage i) $ rs

toSubImage :: Image       -- ^ Image to take from
           -> AtlasRegion -- ^ Region to take
           -> (String, Image) -- Name of the region, The image of the region
toSubImage img (AtlasRegion n l r t b) =
  let (V2 srcW srcH) = img^. Img.texture . Tex.srcSize
      (V4 tx tw ty th) = (fromIntegral <$> V4 l (r-l) t (b-t)) / V4 srcW srcW srcH srcH
      subImg = img & (Img.texture . Tex.position .~ V2 tx ty)
                   . (Img.texture . Tex.size .~ V2 tw th)
  in (n, subImg)

loadAtlas :: FilePath -> IO [AtlasRegion]
loadAtlas path = do
  eitherAppConfig <- decodeFileEither path
  return $ either (error . show) id eitherAppConfig

drawRegion :: V2 Int     -- ^ Screen size
           -> ImageAtlas -- ^ Atlas to use
           -> String     -- ^ Name of sub image in atlas
           -> IO ()
drawRegion ss ia n =
  maybe (error $ "Cannot find \"" ++ n ++ "\" in texture atlas.")
        (Img.draw ss)
        $ HM.lookup n ia
