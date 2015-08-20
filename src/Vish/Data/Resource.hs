module Vish.Data.Resource where

import Vish.Script
import System.FilePath

bgDirectory :: FilePath
bgDirectory =
  "data" </> "background"

actorDirectory :: Name -> FilePath
actorDirectory name =
  "data" </> "actor" </> name

gameConfigFile :: FilePath
gameConfigFile = "data" </> "Vish.yaml"
