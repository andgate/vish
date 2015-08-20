module Vish.Graphics.Util where

import Unsafe.Coerce
import qualified Graphics.Rendering.OpenGL.GL as GL
import Graphics.Rendering.OpenGL.GL (($=))

import Linear.V2 (V2 (..))

gf :: Float -> GL.GLfloat
gf = unsafeCoerce
{-# INLINE gf #-}

gsizei :: Int -> GL.GLsizei
gsizei = unsafeCoerce
{-# INLINE gsizei #-}

-- | Set up the OpenGL rendering context for orthographic projection and run an
--   action to draw the model.
withModelview :: V2 Int -> IO () -> IO ()
withModelview size action = do
  let V2 sx sy = fromIntegral <$> size
  GL.matrixMode   $= GL.Projection
  GL.preservingMatrix $ do
    GL.loadIdentity
    GL.ortho 0 sx sy 0 0 (-1)
    GL.matrixMode $= GL.Modelview 0
    action
    GL.matrixMode $= GL.Projection
  GL.matrixMode $= GL.Modelview 0
