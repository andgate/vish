module Vish.Graphics.Picture where

import Vish.Graphics.Data.Picture
import Vish.Graphics.Texture
import Vish.Graphics.Data.Texture
import Vish.Graphics.Util

import qualified Graphics.Rendering.OpenGL.GL as GL

displayPicture :: TexCache -> Picture -> IO ()
displayPicture texCache pic =
  withModelview (640, 480) $ drawPicture texCache pic

drawPicture :: TexCache -> Picture -> IO ()
drawPicture texCache picture =
  case picture of
    Blank ->
      return ()

    Image path next -> do
      eitherTex <- fetchTexture texCache path
      case eitherTex of
        Left _ -> return ()
        Right tex -> drawTexture tex
      drawPicture texCache next

    Translate (Vector2f x y) next ->
      GL.preservingMatrix $ do
        GL.translate (GL.Vector3 (gf x) (gf y) 0)
        drawPicture texCache next

    Rotate deg next ->
      GL.preservingMatrix $ do
        GL.rotate (gf deg) (GL.Vector3 0 0 (-1))
        drawPicture texCache next

    Scale (Vector2f x y) next ->
      GL.preservingMatrix $ do
        GL.scale (gf x) (gf y) 1
        drawPicture texCache next

    Pictures pics ->
      mapM_ (drawPicture texCache) pics

    _ ->
      return ()
