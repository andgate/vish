{-# LANGUAGE CPP #-}
module Vish.Application.Backend
  ( module Vish.Application.Backend.Types
#ifdef WITHGLFW
  , module Vish.Application.Backend.GLFW
#elif WITHGLUT
  , module Vish.Application.Backend.GLUT
#endif
  , defaultBackendState
  )
where

import Vish.Application.Backend.Types

#ifdef WITHGLFW
import Vish.Application.Backend.GLFW
#elif WITHGLUT
import Vish.Application.Backend.GLUT
#endif

#ifdef WITHGLFW
defaultBackendState :: GLFWState
#elif WITHGLUT
defaultBackendState :: GLUTState
#else
#error No default backend defined
#endif
defaultBackendState = initBackendState
