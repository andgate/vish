{-# LANGUAGE LambdaCase #-}
module Vish.Resource
  ( module Vish.Resource
  , module Vish.Data.Resource
  )
where

import Vish.Data.Resource

import Vish.Script

import Vish.Graphics.Data.Texture
import Vish.Graphics.Texture

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
                    ++ "\nFiles found: " ++ (intercalate ", ") contents
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

installActorTexture :: TexCache -> Actor -> IO ()
installActorTexture texCache actor = do
  let tag = actorTag actor
  path <- findActorFile actor
  installTexture texCache path tag

installScriptActors :: TexCache -> [ScriptCommand] -> IO ()
installScriptActors texCache =
  mapM_ (installActorTexture texCache) . getActors

installBgTexture :: TexCache -> Background -> IO ()
installBgTexture texCache name = do
  path <- findBgFile name
  installTexture texCache path name

installScriptBgs :: TexCache -> [ScriptCommand] -> IO ()
installScriptBgs texCache =
  mapM_ (installBgTexture texCache) . getBgs

initScript :: TexCache -> [ScriptCommand] -> IO ()
initScript texCache commands = do
  scrubTexCache texCache -- Needs to be clean for new script
  installScriptBgs texCache commands
  installScriptActors texCache commands
