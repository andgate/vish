module Vish.Application.AppConfig
  ( module Vish.Application.AppConfig
  , module Vish.Application.Data.AppConfig
  )
where

import Vish.Application.Data.AppConfig
import Vish.Application.Data.App
import Vish.Application.Data.Window

import Data.Yaml


loadAppConfig :: IO AppConfig
loadAppConfig = loadAppConfigFile appConfigFile

loadAppConfigFile :: String -> IO AppConfig
loadAppConfigFile path = do
  eitherAppConfig <- decodeFileEither path
  return $ either (error . show) id eitherAppConfig

appConfigToWindow :: AppConfig -> Window
appConfigToWindow appConfig =
  let name = appConfigName appConfig
      width = appConfigWindowWidth appConfig
      height = appConfigWindowHeight appConfig
      isFullscreen =
        if appConfigIsFullscreen appConfig
          then WindowFullscreen
          else WindowFloating
  in Window
  { _windowName = name
  , _windowX = 0
  , _windowY = 0
  , _windowWidth = width
  , _windowHeight = height
  , _windowState = isFullscreen
  }
