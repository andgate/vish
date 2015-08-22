module Vish.Graphics.Data.ImageAtlas where

import Vish.Graphics.Image (Image)
import qualified Vish.Graphics.Image as Img

import Data.HashMap.Strict (HashMap)
import Data.Yaml

import Linear.V4 (V4 (..))

type ImageAtlas = HashMap String Image

data AtlasRegion =
  AtlasRegion
    { _name           :: String
    , _leftSection    :: Int
    , _rightSection   :: Int
    , _topSection     :: Int
    , _bottomSection  :: Int
    }

instance FromJSON AtlasRegion where
  parseJSON (Object v) = AtlasRegion <$>
                           v .: "name" <*>
                           v .: "left" <*>
                           v .: "right" <*>
                           v .: "top" <*>
                           v .: "bottom"
    -- A non-Object value is of the wrong type, so fail.
  parseJSON _ = error "Can't parse MyUser from YAML/JSON"
