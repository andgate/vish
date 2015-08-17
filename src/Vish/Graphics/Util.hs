module Vish.Graphics.Util where

import Unsafe.Coerce
import qualified Graphics.Rendering.OpenGL.GL as GL
import Graphics.Rendering.OpenGL.GL (($=))

import Linear.V2 (V2 (..))
import qualified Linear.V2 as Vec

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
  GL.matrixMode   $= GL.Projection
  GL.preservingMatrix $ do
    GL.loadIdentity
    let V2 sx sy = fromIntegral <$> size
    GL.ortho 0 sx sy 0 0 (-100)
    GL.matrixMode $= GL.Modelview 0
    action
    GL.matrixMode $= GL.Projection
  GL.matrixMode $= GL.Modelview 0
