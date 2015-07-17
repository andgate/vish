module Vish.Interpreter where

import Vish.Script
import Vish.Graphics.Data.Texture
import Vish.Graphics.Texture
import Vish.Graphics.Picture
import Vish.Util
import Data.Maybe
import System.Directory
import System.FilePath
import Text.Regex.TDFA

actorDirectory :: Name -> FilePath
actorDirectory name =
  "data" </> "actor" </> name

actorRegex :: Name -> Expression -> String
actorRegex name expr =
  actorDirectory name </> name ++ "-" ++ expr <.> "*"

bgDirectory :: FilePath
bgDirectory =
  "data" </> "background"

bgRegex :: Name -> String
bgRegex name =
  bgDirectory </> name <.> "*"

findActorFile :: Actor -> IO FilePath
findActorFile (name, expr) = do
  contents <- getDirectoryContents $ actorDirectory name
  let matches = mapMaybe (=~~ actorRegex name expr) contents
  case matches of
    []  -> error $ "No image found for " ++ name ++ " with expression " ++ expr
    [x] -> return x
    xs -> error $ "File conflicts for " ++ name ++ " with expression " ++ expr ++ ": " ++ show xs

findBgFile :: Name -> IO FilePath
findBgFile name = do
  contents <- getDirectoryContents bgDirectory
  let matches = mapMaybe (=~~ bgRegex name) contents
  case matches of
    []  -> error $ "No image found for background " ++ name
    [x] -> return x
    xs -> error $ "File conflicts for background " ++ name ++ ": " ++ show xs

installActorTexture :: TexCache -> Actor -> IO ()
installActorTexture texCache actor = do
  let tag = actorTag actor
  path <- findActorFile actor
  installTexture texCache path tag

installScriptActors :: TexCache -> Script -> IO ()
installScriptActors texCache script =
  let actors = getActors script
  in mapM_ (installActorTexture texCache) actors

installBgTexture :: TexCache -> Background -> IO ()
installBgTexture texCache name = do
  path <- findBgFile name
  installTexture texCache path name

installScriptBgs :: TexCache -> Script -> IO ()
installScriptBgs texCache script =
  let bgs = getBgs script
  in mapM_ (installBgTexture texCache) bgs
