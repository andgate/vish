module Vish.Data.Config where

import Control.Applicative
import Control.Lens
import Data.Yaml
import System.FilePath

data Config =
  Config
    { _fontName :: String
    , _skinName :: String
    }

instance FromJSON Config where
    parseJSON (Object v) =
      Config <$> v .: "font"
             <*> v .: "skin"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _ = empty

makeLenses ''Config
