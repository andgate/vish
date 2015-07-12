module Vish.Interpreter where

import Vish.Script
import Vish.Graphics.Texture
import Vish.Graphics.Picture
import qualified Data.Set as S
import System.Directory
import System.FilePath

exprToPath :: Name -> Expression -> String
exprToPath name expr =
  "data/actor/" ++ name ++ "/" ++ name ++ "-" ++ expr

exprSetToPaths :: ExprSet -> [String]
exprSetToPaths = S.foldr (\(n, e) p -> exprToPath n e : p) []

extractActorImagePaths :: Script -> [String]
extractActorImagePaths =
  exprSetToPaths . getExpressionSet

loadActorTexture :: String -> IO (Either String Image)
loadActorTexture
