module Vish.Renderer.Picture where

import Vish.Renderer.Data.Picture
import Vish.Renderer.Texture
import Vish.Renderer.Data.Texture
import Vish.Renderer.Util

import Control.Monad
import Graphics.Rendering.OpenGL                        (($=), get)
import qualified Graphics.Rendering.OpenGL.GL           as GL
import qualified Graphics.UI.GLUT                       as GLUT



drawPicture :: TexCache -> Picture -> IO ()
drawPicture texCache picture =
  case picture of
    Blank ->
      return ()
    Translate (Vector2f x y) ->
      GL.preservingMatrix $ do
        GL.translate (GL.Vector3 (gf x) (gf y) 0)
        drawPicture texCache picture

    Image (Vector2f w h) path -> do
      tex <- loadTexture texCache path
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
          (imagePath w h)
          [(0, 0), (1.0, 0), (1.0, 1.0), (0, 1.0)]

      GL.currentColor $= oldColor

      GL.texture GL.Texture2D $= GL.Disabled
    Pictures ps ->
      mapM_ (drawPicture texCache) ps
    _ ->
      return ()


imagePath :: Float -> Float -> [(Float, Float)]
imagePath w h =
  [(0, 0), (w, 0), (w, h), (0,h)]
