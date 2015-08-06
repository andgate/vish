module Vish.Application.App where

import           Vish.Application.Data.App
import           Vish.Application.Internal.App
import           Vish.Application.Internal.Backend

import           Control.Concurrent
import           Control.Lens
import           Data.IORef
import           Vish.Application.Data.IORef.Lens


play :: AppListener w => w -> IO ()
play = playWithBackend defaultBackendState

quitApp :: AppRef a -> IO ()
quitApp appRef =
  appRef & appStatus @~ AppQuit

appDelay :: Double -> IO ()
appDelay s = threadDelay ms
  where ms = round $ s * 1000000
