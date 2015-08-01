module Vish.Application.Data.Window where

import Control.Applicative
import Control.Lens
import Data.Yaml
import qualified Data.Char as C
import qualified Data.HashMap.Lazy as HML
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

data WindowState = Windowed | FullScreen
  deriving (Show)

data Window = Window
  { _winName :: String,
    _winX :: Int,
    _winY :: Int,
    _winW :: Int,
    _winH :: Int,
    _winState :: WindowState
  }

windowDefault :: Window
windowDefault =
  Window
  { _winName = "default",
    _winX = 0,
    _winY = 0,
    _winW = 640,
    _winH = 480,
    _winState = Windowed
  }

makeLenses ''Window

windowConfigPath :: String
windowConfigPath = "data/config/Window.yml"

loadWindow :: IO Window
loadWindow = loadWindowConfig windowConfigPath

loadWindowConfig :: String -> IO Window
loadWindowConfig path = do
  eitherWinConfig <- decodeFileEither path
  return $ either (error . show) id eitherWinConfig

instance FromJSON WindowState where
  parseJSON (Object v) =
    case HML.lookup (T.pack "type") v of
      Just (String s) -> fromString (TL.unpack (TL.fromStrict s))
      _ -> empty
    where
      fromString :: String -> Parser WindowState
      fromString s =
        case map C.toLower s of
          "windowed"   -> pure Windowed
          "fullscreen" -> pure FullScreen
          _ -> empty

  parseJSON _ = empty

instance FromJSON Window where
    parseJSON (Object v) =
      Window <$>
        v .: "name" <*>
        v .: "x" <*>
        v .: "y" <*>
        v .: "w" <*>
        v .: "h" <*>
        v .: "state"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _ = empty
