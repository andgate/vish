module Vish.Interpreter
  ( module Vish.Interpreter
  , module Vish.Data.Interpreter
  )
where

import Vish.Data.Interpreter

import qualified Vish.Data.Config as Cfg
import Vish.Resource

import Vish.Script (Script, ScriptCommand, Name, Actor)
import qualified Vish.Script as S


import GXK.App
import GXK.Data.App
import GXK.Input
import GXK.Window

import Vish.Stage (Stage (..), stageCenter, stageRight, stageLeft, stageBackground)
import qualified Vish.Stage as Stage

import qualified Vish.Graphics as Graphics
import Vish.Graphics.Image (Image (..))
import qualified Vish.Graphics.Image as Img
import qualified Vish.Graphics.ImageAtlas as ImgAtlas
import qualified Vish.Graphics.Texture as Tex


import Control.Lens
import GXK.Data.IORef.Lens
import Control.Monad
import qualified Data.List.Zipper as Z

import Linear.V2 (V2 (..))

instance AppListener Interpreter where
  appCreate appRef = do
    Graphics.init

    (winW, winH) <- appRef ^@ appWindow.windowSize
    appRef & appWorld . interpreterStage @%= Stage.layout (V2 winW winH)

    loadMessageBoxSkin appRef

  appUpdate appRef = do
    isWaiting <- appRef ^@ appWorld.interpreterWaiting
    unless isWaiting $ scriptUpdate appRef
    registerInputListener appRef $ InterpreterInput appRef

  appDraw appRef = do
    stage <- appRef ^@ appWorld.interpreterStage

    Graphics.startDraw
    Stage.draw stage
    Graphics.endDraw

  appDispose appRef =
    return ()

  appPause _ =
    return ()

  appResume _ =
    return ()

  appResize appRef wS = do
    Graphics.resize wS

    appRef & appWorld.interpreterStage @%= Stage.layout wS


instance InputListener InterpreterInput where
  mouseClicked (InterpreterInput appRef) _ _ =
    appRef & appWorld.interpreterWaiting @~ False
  keyReleased (InterpreterInput appRef) Key'Enter =
    appRef & appWorld.interpreterWaiting @~ False
  keyReleased (InterpreterInput appRef) Key'Space =
    appRef & appWorld.interpreterWaiting @~ False
  keyReleased (InterpreterInput appRef) Key'F11 =
    appRef & appWorld.interpreterWaiting @~ False
  keyReleased (InterpreterInput appRef) Key'Escape =
      quitApp appRef
  keyReleased _ _ = return ()

scriptUpdate :: AppRef Interpreter -> IO ()
scriptUpdate appRef = do
  commands <- appRef ^@ appWorld.interpreterCommands
  let commands' = Z.right commands
      maybeCommand = Z.safeCursor commands

  -- Save the freshly moved zipper
  appRef & appWorld.interpreterCommands @~ commands'

  case maybeCommand of
    Nothing -> quitApp appRef
    Just command -> commandUpdate appRef command

commandUpdate :: AppRef Interpreter -> ScriptCommand -> IO ()
commandUpdate appRef command =
  case command of
    S.Done -> quitApp appRef
    S.SetBackground name _ -> setBackground appRef name
    S.Pause t _ -> appDelay t
    S.Speak a m _ -> actorSpeak appRef a m
    S.ShowActor c _ -> showCenterActor appRef c
    S.ShowActors l r _ -> showActors appRef l r
    _ -> print command

setBackground :: AppRef Interpreter -> Name -> IO ()
setBackground appRef name = do
  appRef & appWorld.interpreterStage @%= Stage.clearBackground
  texBgPath <- findBgFile name
  bgImg <- liftM Img.mkImage $ Tex.load texBgPath
  appRef & appWorld.interpreterStage @%= Stage.setBackground bgImg


showCenterActor :: AppRef Interpreter -> Actor -> IO ()
showCenterActor appRef actor = do
  clearActors appRef
  showActor appRef actor Stage.setCenter

showActors :: AppRef Interpreter -> Actor -> Actor -> IO ()
showActors appRef actorL actorR = do
  clearActors appRef
  showActor appRef actorL Stage.setLeft
  showActor appRef actorR Stage.setRight

showActor :: AppRef Interpreter -> Actor -> (Image -> Stage -> IO Stage) -> IO ()
showActor appRef actor stageSetter= do
  actorTexPath <- findActorFile actor
  actorImg <- liftM Img.mkImage $ Tex.load actorTexPath
  appRef & appWorld.interpreterStage @%= stageSetter actorImg

clearActors :: AppRef Interpreter -> IO ()
clearActors appRef = do
  appRef & appWorld.interpreterStage @%= Stage.clearActors


actorSpeak :: AppRef Interpreter -> Name -> String -> IO ()
actorSpeak appRef name msg = do
  appRef & appWorld.interpreterWaiting @~ True
  setMessage appRef msg

setMessage :: AppRef Interpreter -> String -> IO ()
setMessage appRef msg = do
  appRef & appWorld.interpreterStage @%= Stage.clearMessage
  appRef & appWorld.interpreterStage @%= Stage.setMessage msg

loadMessageBoxSkin :: AppRef Interpreter -> IO ()
loadMessageBoxSkin appRef = do
  skinName <- appRef ^@ appWorld . interpreterConfig. Cfg.skinName
  skinImageFilename <- findSkinImage skinName

  msgboxAtlas <- ImgAtlas.load skinImageFilename (atlasFile skinName)

  appRef & appWorld . interpreterStage @%= Stage.setMsgBoxSkin msgboxAtlas
