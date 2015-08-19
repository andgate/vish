module Vish.Game
  ( module Vish.Game
  , module Vish.Data.Game
  )
where

import Vish.Data.Game
import Vish.Script
import Vish.Interpreter

import GXK.App
import GXK.Data.App
import GXK.Input
import GXK.Window

import Vish.Stage (Stage (..), stageMsgBox)
import qualified Vish.Stage as Stage

import Vish.MessageBox (MessageBox)
import qualified Vish.MessageBox as MsgBox

import qualified Vish.Graphics as Graphics
import qualified Vish.Graphics.Data.Color as C

import Vish.Graphics.Image (Image (..))
import qualified Vish.Graphics.Image as Img

import Vish.Graphics.Texture

import Vish.Graphics.Font (Font)
import qualified Vish.Graphics.Font as Font

import Control.Lens
import GXK.Data.IORef.Lens
import Control.Monad
import Data.Monoid
import Data.IORef
import qualified Data.List.Zipper as Z

import Linear.V2 (V2 (..))
import qualified Linear.V2 as Vec
import qualified Linear.Vector as Vec

instance AppListener GameWorld where
  appCreate appRef = do
    Graphics.init

    (winW, winH) <- appRef ^@ appWindow.windowSize
    appRef & appWorld . gameStage @%~ Stage.setSize (V2 winW winH)

    loadScript appRef

  appUpdate appRef = do
    isWaiting <- appRef ^@ appWorld.gameWaiting
    unless isWaiting $ scriptUpdate appRef
    registerInputListener appRef $ GameInput appRef

  appDraw appRef = do
    stage <- appRef ^@ appWorld.gameStage

    Graphics.startDraw
    Stage.draw stage
    Graphics.endDraw

  appDispose _ =
    print "Disposing app"

  appPause _ =
    print "Application paused."

  appResume _ =
    print "Application resumed."

  appResize appRef (winW, winH) = do
    let s = V2 winW winH

    Graphics.resize s

    appRef & appWorld.gameStage @%= Stage.resize s

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
    ShowActor c _ -> gameShowCenterActor appRef c
    ShowActors l r _ -> gameShowActors appRef l r
    SetFont fntName _ -> gameSetFont appRef fntName
    _ -> print command

loadScript :: AppRef GameWorld -> IO ()
loadScript appRef = do
  texCache <- appRef ^@ appWorld.gameTexCache
  commands <- appRef ^@ appWorld.gameCommands
  initScript texCache $ Z.toList commands

gameSetBackground :: AppRef GameWorld -> Name -> IO ()
gameSetBackground appRef name = do
  texCache <- appRef ^@ appWorld.gameTexCache
  eitherBgTex <- fetchTexture texCache name
  case eitherBgTex of
    Left msg -> print msg
    Right bgTex ->
      let bgImg = Img.mkImage bgTex
      in appRef & appWorld.gameStage @%~ Stage.setBackground bgImg


gameShowCenterActor :: AppRef GameWorld -> Actor -> IO ()
gameShowCenterActor appRef actor =
  gameShowActor appRef actor Stage.setCenter

gameShowActors :: AppRef GameWorld -> Actor -> Actor -> IO ()
gameShowActors appRef actorL actorR = do
  gameShowLeftActor appRef actorL
  gameShowRightActor appRef actorR

gameShowLeftActor :: AppRef GameWorld -> Actor -> IO ()
gameShowLeftActor appRef actor =
  gameShowActor appRef actor Stage.setLeft

gameShowRightActor :: AppRef GameWorld -> Actor -> IO ()
gameShowRightActor appRef actor =
  gameShowActor appRef actor Stage.setRight

gameShowActor :: AppRef GameWorld -> Actor -> (Image -> Stage -> Stage) -> IO ()
gameShowActor appRef actor stageSetter= do
  texCache <- appRef ^@ appWorld.gameTexCache
  eitherActorTex <- fetchTexture texCache $ actorTag actor
  case eitherActorTex of
    Left msg -> print msg
    Right actorTex ->
      let actorImg = Img.mkImage actorTex
      in appRef & appWorld.gameStage @%~ stageSetter actorImg

gameActorSpeak :: AppRef GameWorld -> Name -> String -> IO ()
gameActorSpeak appRef name msg = do
  appRef & appWorld.gameWaiting @~ True
  --appRef & appWorld.gameStagePic @~ actorPic
  gameBuildMessage appRef msg
  print $ name ++ ": " ++ msg

gameSetFont :: AppRef GameWorld -> String -> IO ()
gameSetFont appRef fntName = do
  let fontPath = "data/font/" ++ fntName ++ ".ttf"
  fnt <- Font.load fontPath
  let msgBox = MsgBox.mkMsgBox fnt
  appRef & appWorld.gameStage @%= Stage.setMsgBox msgBox

gameBuildMessage :: AppRef GameWorld -> String -> IO ()
gameBuildMessage appRef msg = do
  appRef & appWorld.gameStage @%= Stage.setMessage msg
