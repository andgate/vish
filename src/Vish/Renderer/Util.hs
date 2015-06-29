module Vish.Renderer.Util where

import Unsafe.Coerce
import qualified Graphics.Rendering.OpenGL.GL as GL

gf :: Float -> GL.GLfloat
gf x = unsafeCoerce x
{-# INLINE gf #-}
