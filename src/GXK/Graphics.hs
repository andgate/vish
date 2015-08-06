module Vish.Application.Graphics where

import Graphics.Rendering.OpenGL (get, ($=))
import qualified Graphics.Rendering.OpenGL as GL

initGraphics :: IO ()
initGraphics = do
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  GL.color $ GL.Color4 0 0 0 (1 :: GL.GLfloat)
  GL.blend $= GL.Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.depthFunc $= Just GL.Always

renderStart :: IO ()
renderStart = do
  GL.color $ GL.Color4 0 0 0 (1 :: GL.GLfloat)
  GL.clear [GL.ColorBuffer]

renderEnd :: IO ()
renderEnd = return ()
