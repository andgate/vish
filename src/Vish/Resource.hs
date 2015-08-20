{-# LANGUAGE LambdaCase #-}
module Vish.Resource
  ( module Vish.Resource
  , module Vish.Data.Resource
  )
where

import Vish.Data.Resource

import Vish.Script

import Data.Maybe
import Data.List
import System.Directory
import System.FilePath
import Text.Regex.TDFA


actorRegex :: Name -> Expression -> String
actorRegex _ expr =
  expr <.> "*"

bgRegex :: Name -> String
bgRegex name =
  name <.> "*"

findActorFile :: Actor -> IO FilePath
findActorFile (name, expr) = do
  contents <- getDirectoryContents $ actorDirectory name
  let matches = mapMaybe (=~~ actorRegex name expr) contents
  case matches of
    []  -> error $ "No image found for " ++ name ++ " with expression " ++ expr
                    ++ "\nPath regex: " ++ actorRegex name expr
                    ++ "\nFiles found: " ++ intercalate ", " contents
    [x] -> return $ actorDirectory name </> x
    xs -> error $ "File conflicts for " ++ name ++ " with expression " ++ expr ++ ": " ++ show xs

findBgFile :: Name -> IO FilePath
findBgFile name = do
  contents <- getDirectoryContents bgDirectory
  let matches = mapMaybe (=~~ bgRegex name) contents
  case matches of
    []  -> error $ "No image found for background " ++ name
    [x] -> return $ bgDirectory </> x
    xs -> error $ "File conflicts for background " ++ name ++ ": " ++ show xs
