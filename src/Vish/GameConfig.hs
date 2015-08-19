module Vish.GameConfig
  ( module Vish.GameConfig
  , module Vish.Data.GameConfig
  )
where

import Vish.Data.GameConfig
import Vish.Data.Game

import Vish.Script

import qualified Vish.Graphics.Font as Font

import Data.Yaml
import System.FilePath

load :: IO GameConfig
load = loadFile gameConfigFile

loadFile :: String -> IO GameConfig
loadFile path = do
  eitherAppConfig <- decodeFileEither path
  return $ either (error . show) id eitherAppConfig

toGame :: Script -> GameConfig -> IO GameWorld
toGame script gameCfg = do
  let fntName = gameConfigFontName gameCfg
      fntPath = "data/font/" ++ fntName ++ ".ttf"
  fnt <- Font.load fntPath
  mkGameWorld script fnt
