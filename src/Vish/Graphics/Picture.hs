module Vish.Graphics.Picture where

import Vish.Graphics.Data.Picture
import Vish.Graphics.Texture
import Vish.Graphics.Data.Texture
import Vish.Graphics.Util

import Control.Monad
import Control.Monad.Free
import Graphics.Rendering.OpenGL                        (($=), get)
import qualified Graphics.Rendering.OpenGL.GL           as GL
import qualified Graphics.UI.GLUT                       as GLUT



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

    Pictures pics ->
      mapM_ (drawPicture texCache) pics

    _ ->
      return ()
