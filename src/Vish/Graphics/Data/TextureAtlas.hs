module Vish.Graphics.Data.TextureAtlas where

import Vish.Graphics.Data.Texture

import Data.HashMap.Strict (HashMap)

import Linear.V4 (V4 (..))

import Data.Yaml

data TextureAtlas =
  TextureAtlas
    { texture :: Texture
    , regions :: TextureRegions
    }

type TextureRegions = HashMap String (V4 Float)

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
