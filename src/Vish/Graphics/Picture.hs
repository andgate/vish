module Vish.Graphics.Picture
  ( module Vish.Graphics.Picture
  , module Vish.Graphics.Data.Picture
  )
where

import Vish.Graphics.Data.Picture
import Vish.Graphics.Texture
import Vish.Graphics.Util
import Vish.Math.Vector (Vector2f (..))
import qualified Vish.Math.Vector as Vec

import Data.Maybe
import qualified Data.HashTable.IO as H

import qualified Graphics.Rendering.OpenGL.GL as GL

displayPicture :: TexCache -> Picture -> (Int, Int) -> IO ()
displayPicture texCache pic screenSize =
  withModelview screenSize $ drawPicture texCache pic

drawPicture :: TexCache -> Picture -> IO ()
drawPicture texCache pic =
  drawPictureXYWH texCache pic Nothing Nothing []

drawPictureXYWH :: TexCache -> Picture -> Maybe Vector2f -> Maybe Vector2f -> [DrawFlag] -> IO ()
drawPictureXYWH texCache pic maybePos maybeSize flags =
  case pic of
    Blank ->
      return ()

    Image name next -> do
      eitherTex <- fetchTexture texCache name
      case eitherTex of
        Left e -> print e
        Right tex -> do
          let pos = fromMaybe Vec.zero maybePos
              size = fromMaybe (texSize tex) maybeSize
          drawTexXYWH tex pos size

      drawPictureXYWH texCache next maybePos maybeSize flags

    SetPosition v next ->
      let maybePos' = Just v
      in drawPictureXYWH texCache next maybePos' maybeSize flags

    Move v next ->
      let maybePos' = Just $ maybe v (+ v) maybePos
      in drawPictureXYWH texCache next maybePos' maybeSize flags

    SetSize v next ->
      let maybeSize' = Just v
      in drawPictureXYWH texCache next maybePos maybeSize' flags

    AddFlag flag next ->
      let flags' = flag : flags
      in drawPictureXYWH texCache next maybePos maybeSize flags'

    Stretch v next ->
      let maybeSize' = Just $ maybe v (+ v) maybeSize
      in drawPictureXYWH texCache next maybePos maybeSize' flags

    Translate (Vector2f x y) next ->
      GL.preservingMatrix $ do
        GL.translate (GL.Vector3 (gf x) (gf y) 0)
        drawPictureXYWH texCache next maybePos maybeSize flags

    Rotate deg next ->
      GL.preservingMatrix $ do
        GL.rotate (gf deg) (GL.Vector3 0 0 (-1))
        drawPictureXYWH texCache next maybePos maybeSize flags

    Scale (Vector2f x y) next ->
      GL.preservingMatrix $ do
        GL.scale (gf x) (gf y) 1
        drawPictureXYWH texCache next maybePos maybeSize flags

    Pictures pics ->
      mapM_ (\pic -> drawPictureXYWH texCache pic maybePos maybeSize flags) pics

    _ ->
      return ()
