module Vish.Interpreter where

import Vish.Script
import Vish.Graphics.Texture
import Vish.Graphics.Picture
import Data.Set

getCharacters :: Script -> [(Actor, Expressions)]
getCharacters = undefined . scriptToList
  where isShow :: Command () -> Bool
        isShow (ShowActor _ _) = True
        isShow (ShowActors _ _) = True
