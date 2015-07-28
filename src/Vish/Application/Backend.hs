{-# LANGUAGE CPP #-}
module Vish.Application.Backend
  ( module Vish.Application.Backend.Types
#ifdef WITHGLUT
  , module Vish.Application.Backend.GLUT
#endif
  , defaultBackendState
  )
where

import Vish.Application.Backend.Types

#ifdef WITHGLUT
import Vish.Application.Backend.GLUT
#endif

#ifdef WITHGLUT
defaultBackendState :: GLUTState
#else
#error No default backend defined
#endif
defaultBackendState = initBackendState
