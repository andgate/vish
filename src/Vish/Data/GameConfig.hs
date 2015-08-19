module Vish.Data.GameConfig where

import Control.Applicative
import Data.Yaml
import System.FilePath


data GameConfig =
  GameConfig
    { gameConfigFontName :: String
    }

gameConfigFile :: FilePath
gameConfigFile = "data" </> "game.yaml"

instance FromJSON GameConfig where
    parseJSON (Object v) =
      GameConfig <$>
        v .: "font"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _ = empty
