module Vish.Game where

import Vish.Script
import Vish.Interpreter

import GXK.App
import GXK.Data.App
import GXK.Input
import GXK.Graphics.Data.Picture (Picture)
import qualified GXK.Graphics.Data.Picture as Pic
import GXK.Graphics.Data.Texture
import GXK.Graphics.Texture

import Control.Lens
import GXK.Data.IORef.Lens
import Control.Monad
import Data.Monoid
import Data.IORef
import qualified Data.List.Zipper as Z

data GameWorld = GameWorld
  { _gameCommands :: Z.Zipper ScriptCommand
  , _gameBackgroundPic :: Picture
  , _gameStagePic :: Picture
  , _gameWaiting :: Bool
  }

mkGameWorld :: Script -> GameWorld
mkGameWorld script =
  let script' = scriptToZipper script
  in
  GameWorld
  { _gameCommands = script'
  , _gameBackgroundPic = Pic.blank
  , _gameStagePic = Pic.blank
  , _gameWaiting = False
  }

makeLenses ''GameWorld

instance AppListener GameWorld where
  appCreate = loadScript

  appUpdate appRef = do
    isWaiting <- appRef ^@ appWorld.gameWaiting
    unless isWaiting $ scriptUpdate appRef
    registerInputListener appRef $ GameInput appRef

  appDraw appRef = do
    background <- appRef ^@ appWorld.gameBackgroundPic
    stage      <- appRef ^@ appWorld.gameStagePic
    return $ background <> stage

  appDispose _ =
    print "Disposing app"

  appPause _ =
    print "Application paused."

  appResume _ =
    print "Application resumed."

data GameInput = GameInput (AppRef GameWorld)

instance InputListener GameInput where
  mouseClicked (GameInput appRef) _ _ =
    appRef & appWorld.gameWaiting @~ False
  keyReleased (GameInput appRef) Key'Enter =
    appRef & appWorld.gameWaiting @~ False
  keyReleased (GameInput appRef) Key'Space =
    appRef & appWorld.gameWaiting @~ False
  keyReleased (GameInput appRef) Key'F11 =
    appRef & appWorld.gameWaiting @~ False
  keyReleased (GameInput appRef) Key'Escape =
      quitApp appRef
  keyReleased _ _ = return ()


runScript :: Script -> IO ()
runScript = play . mkGameWorld

scriptUpdate :: AppRef GameWorld -> IO ()
scriptUpdate appRef = do
  commands <- appRef ^@ appWorld.gameCommands
  let commands' = Z.right commands
      maybeCommand = Z.safeCursor commands

  -- Save the freshly moved zipper
  appRef & appWorld.gameCommands @~ commands'

  case maybeCommand of
    Nothing -> quitApp appRef
    Just command -> commandUpdate appRef command

commandUpdate :: AppRef GameWorld -> ScriptCommand -> IO ()
commandUpdate appRef command =
  case command of
    Done -> quitApp appRef
    SetBackground name _ -> gameSetBackground appRef name
    Pause t _ -> appDelay t
    Speak a m _ -> gameActorSpeak appRef a m
    ShowActor c _ -> gameShowActor appRef c
    _ -> print command

loadScript :: AppRef GameWorld -> IO ()
loadScript appRef = do
  texCache <- appRef ^@ appGfx.gfxTexCache
  commands <- appRef ^@ appWorld.gameCommands
  initScript texCache $ Z.toList commands

gameSetBackground :: AppRef GameWorld -> Name -> IO ()
gameSetBackground appRef name =
  appRef & appWorld.gameBackgroundPic @~ Pic.image name


gameShowActor :: AppRef GameWorld -> Actor -> IO ()
gameShowActor appRef actor = do
  let actorPic = Pic.image . actorTag $ actor
  appRef & appWorld.gameStagePic @~ actorPic
  return ()

gameActorSpeak :: AppRef GameWorld -> Name -> String -> IO ()
gameActorSpeak appRef name msg = do
  appRef & appWorld.gameWaiting @~ True
  --appRef & appWorld.gameStagePic @~ actorPic
  print $ name ++ ": " ++ msg
