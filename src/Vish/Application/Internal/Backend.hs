{-# LANGUAGE CPP #-}
module Vish.Application.Internal.Backend
  ( module Vish.Application.Internal.Backend.Types
#ifdef WITHGLFW
  , module Vish.Application.Internal.Backend.GLFW
#elif WITHGLUT
  , module Vish.Application.Internal.Backend.GLUT
#endif
  , defaultBackendState
  )
where

import Vish.Application.Internal.Backend.Types

#ifdef WITHGLFW
import Vish.Application.Internal.Backend.GLFW
#elif WITHGLUT
import Vish.Application.Internal.Backend.GLUT
#endif

#ifdef WITHGLFW
defaultBackendState :: GLFWState
#elif WITHGLUT
defaultBackendState :: GLUTState
#else
#error No default backend defined
#endif
defaultBackendState = initBackendState
