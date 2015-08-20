module Vish.Config
  ( module Vish.Config
  , module Vish.Data.Config
  )
where

import Vish.Data.Config
import Vish.Interpreter
import Vish.Script
import Vish.Data.Resource

import qualified Vish.Graphics.Font as Font

import Data.Yaml
import System.FilePath

load :: IO Config
load = loadFile gameConfigFile

loadFile :: FilePath -> IO Config
loadFile path = do
  eitherAppConfig <- decodeFileEither path
  return $ either (error . show) id eitherAppConfig

toInterpreter :: Script -> Config -> IO Interpreter
toInterpreter script gameCfg = do
  let fntName = gameConfigFontName gameCfg
      fntPath = "data/font/" ++ fntName ++ ".ttf"
  fnt <- Font.load fntPath
  mkInterpreter script fnt
