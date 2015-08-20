module Vish.Graphics where

import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL

import Linear.V2 (V2 (..))

init :: IO ()
init = do
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  GL.color $ GL.Color4 0 0 0 (1 :: GL.GLfloat)
  GL.blend $= GL.Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.depthFunc $= Just GL.Always

startDraw :: IO ()
startDraw = do
  GL.color $ GL.Color4 0 0 0 (1 :: GL.GLfloat)
  GL.clear [GL.ColorBuffer]

endDraw :: IO ()
endDraw = return ()

resize :: V2 Int -> IO ()
resize size = do
  let V2 sx sy = fromIntegral <$> size
  GL.viewport $= (GL.Position 0 0, GL.Size sx sy)
