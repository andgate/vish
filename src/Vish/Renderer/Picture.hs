module Vish.Renderer.Picture where

import Vish.Renderer.Data.Picture
import Vish.Renderer.Data.Texture

import Vish.Renderer.Util
import Graphics.Rendering.OpenGL                        (($=), get)
import qualified Graphics.Rendering.OpenGL.GL           as GL
import qualified Graphics.UI.GLUT                       as GLUT



drawPicture :: TexCache -> Picture -> IO ()
drawPicture texCache picture =
  case picture of
    Blank ->
      return ()
    Image name ->
      return ()
    Translate (Vector2i x y) ->
      GL.preservingMatrix $ do
        GL.translate (GL.Vector3 (gf x) (gf y) 0)
        drawPicture texCache picture
    _ ->
      return ()
