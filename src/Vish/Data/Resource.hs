module Vish.Data.Resource where

import Vish.Script
import System.FilePath

bgDirectory :: FilePath
bgDirectory =
  "data" </> "background"

actorDirectory :: Name -> FilePath
actorDirectory name =
  "data" </> "actor" </> name

interpreterConfigFile :: FilePath
interpreterConfigFile =
  "data" </> "Vish.yaml"

skinDirectory :: FilePath
skinDirectory =
  "data" </> "skin"

atlasFile :: String -> FilePath
atlasFile name =
  skinDirectory </> name <.> "atlas"
