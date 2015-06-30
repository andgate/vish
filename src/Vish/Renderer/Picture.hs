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
      eitherTex <- fetchTexture texCache path
      case eitherTex of
        Left _ -> return ()
        Right tex -> drawTexture tex

    Pictures ps ->
      mapM_ (drawPicture texCache) ps
    _ ->
      return ()
