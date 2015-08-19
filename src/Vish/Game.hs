module Vish.Game
  ( module Vish.Game
  , module Vish.Data.Game
  )
where

import Vish.Data.Game

import Vish.Script (Script, ScriptCommand, Name, Actor)
import qualified Vish.Script as S

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

import qualified Vish.Graphics.Texture as Tex

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

  appDispose appRef = do
    texCache <- appRef ^@ appWorld.gameTexCache
    Tex.scrubTexCache texCache

  appPause _ =
    return ()

  appResume _ =
    return ()

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
    S.Done -> quitApp appRef
    S.SetBackground name _ -> setBackground appRef name
    S.Pause t _ -> appDelay t
    S.Speak a m _ -> actorSpeak appRef a m
    S.ShowActor c _ -> showCenterActor appRef c
    S.ShowActors l r _ -> showActors appRef l r
    _ -> print command

loadScript :: AppRef GameWorld -> IO ()
loadScript appRef = do
  texCache <- appRef ^@ appWorld.gameTexCache
  commands <- appRef ^@ appWorld.gameCommands
  initScript texCache $ Z.toList commands

setBackground :: AppRef GameWorld -> Name -> IO ()
setBackground appRef name = do
  texCache <- appRef ^@ appWorld.gameTexCache
  eitherBgTex <- Tex.fetchTexture texCache name
  case eitherBgTex of
    Left msg -> print msg
    Right bgTex ->
      let bgImg = Img.mkImage bgTex
      in appRef & appWorld.gameStage @%~ Stage.setBackground bgImg


showCenterActor :: AppRef GameWorld -> Actor -> IO ()
showCenterActor appRef actor =
  showActor appRef actor Stage.setCenter

showActors :: AppRef GameWorld -> Actor -> Actor -> IO ()
showActors appRef actorL actorR = do
  showLeftActor appRef actorL
  showRightActor appRef actorR

showLeftActor :: AppRef GameWorld -> Actor -> IO ()
showLeftActor appRef actor =
  showActor appRef actor Stage.setLeft

showRightActor :: AppRef GameWorld -> Actor -> IO ()
showRightActor appRef actor =
  showActor appRef actor Stage.setRight

showActor :: AppRef GameWorld -> Actor -> (Image -> Stage -> Stage) -> IO ()
showActor appRef actor stageSetter= do
  texCache <- appRef ^@ appWorld.gameTexCache
  eitherActorTex <- Tex.fetchTexture texCache $ S.actorTag actor
  case eitherActorTex of
    Left msg -> print msg
    Right actorTex ->
      let actorImg = Img.mkImage actorTex
      in appRef & appWorld.gameStage @%~ stageSetter actorImg

actorSpeak :: AppRef GameWorld -> Name -> String -> IO ()
actorSpeak appRef name msg = do
  appRef & appWorld.gameWaiting @~ True
  setMessage appRef msg

setMessage :: AppRef GameWorld -> String -> IO ()
setMessage appRef msg = do
  appRef & appWorld.gameStage @%= Stage.clearMessage
  appRef & appWorld.gameStage @%= Stage.setMessage msg
