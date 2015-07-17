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

findExprFile :: Name -> Expression -> IO FilePath
findExprFile name expr = do
  contents <- getDirectoryContents $ actorDirectory name
  let matches = mapMaybe (=~~ actorExprRegex name expr) contents
  case matches of
    []  -> error $ "No image found for " ++ name ++ " with expression " ++ expr
    x:_ -> return x

loadActorTexture :: Name -> Expression -> IO Texture
loadActorTexture name expr =
  findExprFile name expr >>= loadTexture >>= either error return

loadAllActorTextures :: Script -> IO [(String, Texture)]
loadAllActorTextures script =
  let actorsExprs = S.toList . getExpressionSet $ script
      actorsExprTags = map actorExprTag actorsExprs
  in liftM (zip actorsExprTags) $ mapM (uncurry loadActorTexture) actorsExprs

cacheActorTextures :: TexCache -> Script -> IO ()
cacheActorTextures texCache script = do
  taggedTexs <- loadAllActorTextures script
  mapM_ (uncurry $ cacheTexture texCache) taggedTexs
