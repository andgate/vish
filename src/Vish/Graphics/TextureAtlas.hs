module Vish.Graphics.TextureAtlas
  ( module Vish.Graphics.TextureAtlas
  , module Vish.Graphics.Data.TextureAtlas
  )
where

import Vish.Graphics.Data.TextureAtlas
import qualified Vish.Graphics.Texture as Tex
import Vish.Graphics.Util

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

load :: FilePath -> FilePath -> IO TextureAtlas
load texPath atlasPath = do
  tex <- Tex.load texPath
  atlasRegs <- loadAtlas atlasPath
  let texSize = tex^.Tex.size
      texRegions = HM.fromList . map (toTexCoords texSize) $ atlasRegs
  return $ TextureAtlas tex texRegions


toTexCoords :: V2 Float -> AtlasRegion -> (String, V4 Float)
toTexCoords (V2 texW texH) (AtlasRegion n l r t b) =
  let texRegion = (fromIntegral <$> V4 l r t b) / V4 texW texW texH texH
  in (n, texRegion)

loadAtlas :: FilePath -> IO [AtlasRegion]
loadAtlas path = do
  eitherAppConfig <- decodeFileEither path
  return $ either (error . show) id eitherAppConfig

drawRegionXYWH :: TextureAtlas -> String -> V2 Float -> V2 Float -> IO ()
drawRegionXYWH (TextureAtlas tex rs) n (V2 x y) (V2 w h) = do
  texObjMaybe <- readIORef (tex^.Tex.object)
  case texObjMaybe of
    Nothing -> return ()
    Just _ -> do
      let (V4 tx1 tx2 ty1 ty2) = fromJust $ HM.lookup n rs

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
          imagePath
          [(tx1, ty1), (tx2, ty1), (tx2, ty2), (tx1, ty2)]

      GL.currentColor $= oldColor

      GL.texture GL.Texture2D $= GL.Disabled

  where
    imagePath :: [(Float, Float)]
    imagePath =
      [(x, y), (x+w, y), (x+w, y+h), (x,y+h)]
