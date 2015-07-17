module Vish.Interpreter where


import Control.Monad
import Vish.Script
import Vish.Graphics.Data.Texture
import Vish.Graphics.Texture
import Vish.Graphics.Picture
import Vish.Util
import Data.Maybe
import qualified Data.Set as S
import System.Directory
import System.FilePath
import Text.Regex.TDFA

actorExprRegex :: Name -> Expression -> String
actorExprRegex name expr =
  actorDirectory name </> name ++ "-" ++ expr <.> "*"

actorDirectory :: Name -> FilePath
actorDirectory name =
  "data" </> "actor" </> name </> ""

findExprFile :: (Name, Expression) -> IO FilePath
findExprFile (name, expr) = do
  contents <- getDirectoryContents $ actorDirectory name
  let matches = mapMaybe (=~~ actorExprRegex name expr) contents
  case matches of
    []  -> error $ "No image found for " ++ name ++ " with expression " ++ expr
    [x] -> return x
    xs -> error $ "File conflicts for " ++ name ++ " with expression " ++ expr ++ ": " ++ show xs

installActorTexture :: TexCache -> (Name, Expression) -> IO ()
installActorTexture texCache actorExpr = do
  let tag = actorExprTag actorExpr
  path <- findExprFile actorExpr
  installTexture texCache path tag

installActorTextures :: TexCache -> Script -> IO ()
installActorTextures texCache script =
  let actorsExprs = S.toList . getExpressionSet $ script
  in mapM_ (installActorTexture texCache) actorsExprs
