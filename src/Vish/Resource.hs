{-# LANGUAGE LambdaCase #-}
module Vish.Resource
  ( module Vish.Resource
  , module Vish.Data.Resource
  )
where

import Vish.Data.Resource

import Vish.Script

import qualified Vish.Graphics.Texture as Tex

import Data.Maybe
import Data.List
import System.Directory
import System.FilePath
import Text.Regex.TDFA


actorRegex :: Name -> Expression -> String
actorRegex _ expr =
  expr <.> "*"

fileNameRegex :: Name -> String
fileNameRegex name =
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
  let matches = mapMaybe (=~~ fileNameRegex name) contents
  case matches of
    []  -> error $ "No image found for background " ++ name
    [x] -> return $ bgDirectory </> x
    xs -> error $ "File conflicts for background " ++ name ++ ": " ++ show xs

findSkinImage :: String -> IO FilePath
findSkinImage name = do
  let acceptedFiles = map ((name ++ ".") ++) Tex.supportedExtensions
  contents <- getDirectoryContents skinDirectory
  let matches = concatMap (\x -> filter (==x) acceptedFiles) contents
  case matches of
    []  -> error $ "No image found for skin " ++ name
    [x] -> return $ skinDirectory </> x
    xs -> error $ "File conflicts for skin image " ++ name ++ ": " ++ show xs
