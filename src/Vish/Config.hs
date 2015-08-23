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

import Control.Lens
import Data.Yaml
import System.FilePath


load :: IO Config
load = loadFile interpreterConfigFile

loadFile :: FilePath -> IO Config
loadFile path = do
  eitherAppConfig <- decodeFileEither path
  return $ either (error . show) id eitherAppConfig

toInterpreter :: Script -> Config -> IO Interpreter
toInterpreter script cfg = do
  let fntN = cfg^.fontName
      fntFp = "data/font/" ++ fntN ++ ".ttf"
  fnt <- Font.load fntFp
  mkInterpreter cfg script fnt
