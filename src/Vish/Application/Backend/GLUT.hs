{-# LANGUAGE MultiParamTypeClasses #-}
module Vish.Application.Backend.GLUT
  (GLUTState)
where

import Vish.Application.Backend.Types
import Vish.Application.Data.Input (Key (..), MouseButton (..))
import qualified Vish.Application.Data.Input as I

import Control.Concurrent
import Control.Lens
import Control.Monad
import Data.Bifunctor

import Data.IORef
import Vish.Application.Data.IORef.Lens

import Graphics.UI.GLUT                    (get,($=))
import qualified Graphics.UI.GLUT          as GLUT
import qualified Graphics.Rendering.OpenGL as GL

import qualified System.Exit as System
import qualified System.Mem  as System


-- | We don't maintain any state information for the GLUT backend,
--   so this data type is empty.
data GLUTState =
  GLUTState
  { _glutAppStatus :: GLUTAppStatus
  }

data GLUTAppStatus = GLUTPlay | GLUTPause | GLUTExit

makeLenses ''GLUTState

initGLUTState :: GLUTState
initGLUTState  = GLUTState GLUTPlay

instance Backend GLUTState where
  initBackendState  = initGLUTState
  initializeBackend = initGLUT

  -- non-freeglut doesn't like this: (\_ -> GLUT.leaveMainLoop)
  --exitBackend _ = System.exitWith System.ExitSuccess
  exitBackend = glutAppStatus @~ GLUTExit

  openWindow = openWindowGLUT
  dumpBackendState = dumpStateGLUT

  installCallbacks ref callbacks =
    mapM_ (\f -> f ref callbacks)
      [ installDisplayCallbackGLUT
      , installPauseResumeCallbackGLUT
      , installWindowCloseCallbackGLUT
      , installReshapeCallbackGLUT
      , installKeyboardCallbackGLUT
      , installMouseMoveCallbackGLUT
      , installMouseCallbackGLUT
      ]

  -- Call the GLUT mainloop.
  -- This function will return when something calls GLUT.leaveMainLoop
  runMainLoop _   = GLUT.mainLoop

  getWindowDimensions _ = do
    GL.Size sizeX sizeY <- get GLUT.windowSize
    return . Just $  (fromEnum sizeX, fromEnum sizeY)

  elapsedTime _ =
    liftM ((/ 1000) . fromIntegral) $ get GLUT.elapsedTime

  sleep _ =
    threadDelay . round . (* 1000000)

-- Initialise -----------------------------------------------------------------
initGLUT :: IORef GLUTState -> Bool -> IO ()
initGLUT _ debug = do
  (_progName, _args)  <- GLUT.getArgsAndInitialize

  glutVersion         <- get GLUT.glutVersion
  when debug . putStrLn $ "  glutVersion       = " ++ show glutVersion

  GLUT.initialDisplayMode $= [ GLUT.RGBAMode, GLUT.DoubleBuffered]

  -- See if our requested display mode is possible
  displayMode         <- get GLUT.initialDisplayMode
  displayModePossible <- get GLUT.displayModePossible
  when debug . putStrLn $  "  displayMode       = " ++ show displayMode ++ "\n"
                        ++ "  possible          = " ++ show displayModePossible ++ "\n"


-- Open Window ----------------------------------------------------------------
openWindowGLUT :: IORef GLUTState -> Window -> IO ()
openWindowGLUT _ win = do
  let (x, y) = bimap fromIntegral fromIntegral $ win^.windowPosition
      (w, h) = bimap fromIntegral fromIntegral $ win^.windowSize
  GLUT.initialWindowSize $= GL.Size w h
  GLUT.initialWindowPosition $= GL.Position x y

  GLUT.createWindow $ win^.windowName

  GLUT.windowSize $= GL.Size w h

  case win^.windowState of
    WindowFullscreen -> do
      GLUT.gameModeCapabilities $=
           [ GLUT.Where' GLUT.GameModeWidth GLUT.IsEqualTo $ fromIntegral w
           , GLUT.Where' GLUT.GameModeHeight GLUT.IsEqualTo $ fromIntegral h ]
      void GLUT.enterGameMode
    WindowFloating -> return ()

  --  Switch some things.
  --  auto repeat interferes with key up / key down checks.
  --  BUGS: this doesn't seem to work?
  GLUT.perWindowKeyRepeat   $= GLUT.PerWindowKeyRepeatOff


-- Dump State -----------------------------------------------------------------
dumpStateGLUT
        :: IORef GLUTState
        -> IO ()

dumpStateGLUT _
 = do
        wbw             <- get GLUT.windowBorderWidth
        whh             <- get GLUT.windowHeaderHeight
        rgba            <- get GLUT.rgba

        rgbaBD          <- get GLUT.rgbaBufferDepths
        colorBD         <- get GLUT.colorBufferDepth
        depthBD         <- get GLUT.depthBufferDepth
        accumBD         <- get GLUT.accumBufferDepths
        stencilBD       <- get GLUT.stencilBufferDepth

        doubleBuffered  <- get GLUT.doubleBuffered

        colorMask       <- get GLUT.colorMask
        depthMask       <- get GLUT.depthMask

        putStrLn $ "* dumpGlutState\n"
                ++ "  windowBorderWidth  = " ++ show wbw            ++ "\n"
                ++ "  windowHeaderHeight = " ++ show whh            ++ "\n"
                ++ "  rgba               = " ++ show rgba           ++ "\n"
                ++ "  depth      rgba    = " ++ show rgbaBD         ++ "\n"
                ++ "             color   = " ++ show colorBD        ++ "\n"
                ++ "             depth   = " ++ show depthBD        ++ "\n"
                ++ "             accum   = " ++ show accumBD        ++ "\n"
                ++ "             stencil = " ++ show stencilBD      ++ "\n"
                ++ "  doubleBuffered     = " ++ show doubleBuffered ++ "\n"
                ++ "  mask         color = " ++ show colorMask      ++ "\n"
                ++ "               depth = " ++ show depthMask      ++ "\n"

-- Display Callback -----------------------------------------------------------
installDisplayCallbackGLUT :: IORef GLUTState -> Callbacks -> IO ()
installDisplayCallbackGLUT ref callbacks =
  GLUT.displayCallback $= callbackDisplay ref callbacks

callbackDisplay :: IORef GLUTState -> Callbacks -> IO ()
callbackDisplay ref callbacks = do
  appStatus <- ref ^@ glutAppStatus
  case appStatus of
    GLUTPause -> return ()
    GLUTExit  -> do
      GLUT.leaveGameMode
      maybeWin <- get GLUT.currentWindow
      forM_ maybeWin GLUT.destroyWindow
    GLUTPlay  -> do
      -- get the display callbacks from the chain
      displayCallback callbacks ref

      -- swap front and back buffers
      GLUT.swapBuffers
      -- run gc to reduce pauses during mainloop (hopefully)
      System.performGC

      -- Don't report errors by default.
      -- The windows OpenGL implementation seems to complain for no reason.
      --  GLUT.reportErrors
      GLUT.postRedisplay Nothing

-- App Status Callback -----------------------------------------------------
-- | Callback for when the app is paused/resumed.
installPauseResumeCallbackGLUT :: IORef GLUTState -> Callbacks -> IO ()
installPauseResumeCallbackGLUT ref callbacks =
  GLUT.crossingCallback $= Just (callbackVisibility ref callbacks)

callbackVisibility :: IORef GLUTState -> Callbacks -> GLUT.Crossing -> IO ()
callbackVisibility ref callbacks vis =
  case vis of
    GLUT.WindowEntered -> do
      ref & glutAppStatus @~ GLUTPlay
      resumeCallback callbacks ref
    GLUT.WindowLeft -> do
      ref & glutAppStatus @~ GLUTPause
      pauseCallback callbacks ref


-- Close Callback -------------------------------------------------------------
-- | Callback for when the user closes the window.
--   We can do some cleanup here.
installWindowCloseCallbackGLUT :: IORef GLUTState -> Callbacks -> IO ()
installWindowCloseCallbackGLUT ref callbacks =
  GLUT.closeCallback $= (Just $ closeCallback callbacks ref)

callbackWindowClose :: IORef GLUTState -> Callbacks -> IO ()
callbackWindowClose ref callbacks =
  closeCallback callbacks ref


-- Reshape Callback -----------------------------------------------------------
installReshapeCallbackGLUT :: IORef GLUTState -> Callbacks -> IO ()
installReshapeCallbackGLUT ref callbacks =
  GLUT.reshapeCallback $= Just (callbackReshape ref callbacks)

callbackReshape :: IORef GLUTState -> Callbacks -> GLUT.Size -> IO ()
callbackReshape ref callbacks (GLUT.Size sizeX sizeY) =
  reshapeCallback callbacks ref (fromEnum sizeX) (fromEnum sizeY)


-- Keyboard Callback ----------------------------------------------------------
installKeyboardCallbackGLUT :: IORef GLUTState -> Callbacks -> IO ()
installKeyboardCallbackGLUT ref callbacks = do
  GLUT.keyboardCallback $= Just (callbackKeyboard ref callbacks Down)
  GLUT.keyboardUpCallback $= Just (callbackKeyboard ref callbacks Up)
  GLUT.specialCallback $= Just (callbackSpecialKey ref callbacks Down)
  GLUT.specialUpCallback $= Just (callbackSpecialKey ref callbacks Up)

callbackKeyboard :: IORef GLUTState -> Callbacks -> InputState
                  -> Char -> GLUT.Position -> IO ()
callbackKeyboard ref callbacks state c _ =
  keyboardCallback callbacks ref key state
  where key = fromGLUT c

callbackSpecialKey :: IORef GLUTState -> Callbacks -> InputState
                  -> GLUT.SpecialKey -> GLUT.Position -> IO ()
callbackSpecialKey ref callbacks state key _ =
  keyboardCallback callbacks ref key' state
  where key' = fromGLUT key


-- Move Movement Callback ------------------------------------------------------------
installMouseMoveCallbackGLUT :: IORef GLUTState -> Callbacks -> IO ()
installMouseMoveCallbackGLUT ref callbacks = do
  GLUT.motionCallback        $= Just (callbackMouseMove ref callbacks)
  GLUT.passiveMotionCallback $= Just (callbackMouseMove ref callbacks)

callbackMouseMove :: IORef GLUTState -> Callbacks -> GLUT.Position -> IO ()
callbackMouseMove ref callbacks (GLUT.Position posX posY) =
  mouseMoveCallback callbacks ref (fromIntegral posX) (fromIntegral posY)


-- Mouse Callback ------------------------------------------------------
installMouseCallbackGLUT :: IORef GLUTState -> Callbacks -> IO ()
installMouseCallbackGLUT ref callbacks =
  GLUT.mouseCallback $= Just (callbackMouse ref callbacks)

callbackMouse :: IORef GLUTState -> Callbacks -> GLUT.MouseButton
                  -> GLUT.KeyState -> GLUT.Position -> IO ()
callbackMouse ref callbacks button state (GLUT.Position posX posY) =
  case button of
    GLUT.WheelUp -> scrollCallback callbacks ref 0 1
    GLUT.WheelDown -> scrollCallback callbacks ref 0 (-1)
    _ ->
      mouseButtonCallback callbacks ref button' state' posX' posY'
  where
    button' = fromGLUT button
    state' = fromGLUT state
    posX' = fromIntegral posX
    posY' = fromIntegral posY

-- GLUT type Conversion -------------------------------------------------------
class GLUTConv a b where
  fromGLUT :: a -> b

instance GLUTConv Char Key where
  fromGLUT c =
    case c of
      '\32'  -> Key'Space
      '\13'  -> Key'Enter
      '\9'   -> Key'Tab
      '\ESC' -> Key'Escape
      '\DEL' -> Key'Delete
      'A'    -> Key'A
      'a'    -> Key'a
      'B'    -> Key'B
      'b'    -> Key'b
      'C'    -> Key'C
      'c'    -> Key'c
      'D'    -> Key'D
      'd'    -> Key'd
      'E'    -> Key'E
      'e'    -> Key'e
      'F'    -> Key'F
      'f'    -> Key'f
      'G'    -> Key'G
      'g'    -> Key'g
      'H'    -> Key'H
      'h'    -> Key'h
      'I'    -> Key'I
      'i'    -> Key'i
      'J'    -> Key'J
      'j'    -> Key'j
      'K'    -> Key'K
      'k'    -> Key'k
      'L'    -> Key'L
      'l'    -> Key'l
      'M'    -> Key'M
      'm'    -> Key'm
      'N'    -> Key'N
      'n'    -> Key'n
      'O'    -> Key'O
      'o'    -> Key'o
      'P'    -> Key'P
      'p'    -> Key'p
      'Q'    -> Key'Q
      'q'    -> Key'q
      'R'    -> Key'R
      'r'    -> Key'r
      'S'    -> Key'S
      's'    -> Key's
      'T'    -> Key'T
      't'    -> Key't
      'U'    -> Key'U
      'u'    -> Key'u
      'V'    -> Key'V
      'v'    -> Key'v
      'W'    -> Key'W
      'w'    -> Key'w
      'X'    -> Key'X
      'x'    -> Key'x
      'Y'    -> Key'Y
      'y'    -> Key'y
      'Z'    -> Key'Z
      'z'    -> Key'z
      '`'    -> Key'GraveAccent
      '1'    -> Key'1
      '2'    -> Key'2
      '3'    -> Key'3
      '4'    -> Key'4
      '5'    -> Key'5
      '6'    -> Key'6
      '7'    -> Key'7
      '8'    -> Key'8
      '9'    -> Key'9
      '0'    -> Key'0
      '-'    -> Key'Minus
      '='    -> Key'Equal
      '~'    -> Key'Tilde
      '!'    -> Key'Exclaim
      '@'    -> Key'Ampersand
      '#'    -> Key'Hash
      '$'    -> Key'Dollar
      '%'    -> Key'Percent
      '^'    -> Key'Caret
      '&'    -> Key'Ampersand
      '*'    -> Key'Asterisk
      '('    -> Key'LeftParens
      ')'    -> Key'RightParens
      '_'    -> Key'Underscore
      '+'    -> Key'Plus
      '['    -> Key'LeftBracket
      ']'    -> Key'RightBracket
      '\\'   -> Key'Backslash
      ';'    -> Key'Semicolon
      '\''   -> Key'Apostrophe
      ','    -> Key'Comma
      '.'    -> Key'Period
      '/'    -> Key'Slash
      '{'    -> Key'LeftCurlyBracket
      '}'    -> Key'RightCurlyBracket
      '|'    -> Key'Pipe
      ':'    -> Key'Colon
      '"'    -> Key'Quote
      '<'    -> Key'LesserThan
      '>'    -> Key'GreaterThan
      '?'    -> Key'Question
      _      -> Key'Unknown

instance GLUTConv GLUT.SpecialKey Key where
  fromGLUT key =
    case key of
      GLUT.KeyUnknown _          -> Key'Unknown
      GLUT.KeyF1                 -> Key'F1
      GLUT.KeyF2                 -> Key'F2
      GLUT.KeyF3                 -> Key'F3
      GLUT.KeyF4                 -> Key'F4
      GLUT.KeyF5                 -> Key'F5
      GLUT.KeyF6                 -> Key'F6
      GLUT.KeyF7                 -> Key'F7
      GLUT.KeyF8                 -> Key'F8
      GLUT.KeyF9                 -> Key'F9
      GLUT.KeyF10                -> Key'F10
      GLUT.KeyF11                -> Key'F11
      GLUT.KeyF12                -> Key'F12
      GLUT.KeyLeft               -> Key'Left
      GLUT.KeyUp                 -> Key'Up
      GLUT.KeyRight              -> Key'Right
      GLUT.KeyDown               -> Key'Down
      GLUT.KeyPageUp             -> Key'PageUp
      GLUT.KeyPageDown           -> Key'PageDown
      GLUT.KeyHome               -> Key'Home
      GLUT.KeyEnd                -> Key'End
      GLUT.KeyInsert             -> Key'Insert
      GLUT.KeyNumLock            -> Key'NumLock
      GLUT.KeyBegin              -> Key'RightSuper
      GLUT.KeyDelete             -> Key'Delete
      GLUT.KeyShiftL             -> Key'LeftShift
      GLUT.KeyShiftR             -> Key'RightShift
      GLUT.KeyCtrlL              -> Key'LeftControl
      GLUT.KeyCtrlR              -> Key'RightControl
      GLUT.KeyAltL               -> Key'LeftAlt
      GLUT.KeyAltR               -> Key'RightAlt

instance GLUTConv GLUT.MouseButton MouseButton where
  fromGLUT button =
    case button of
      GLUT.LeftButton           -> Left'Button
      GLUT.MiddleButton         -> Middle'Button
      GLUT.RightButton          -> Right'Button
      GLUT.AdditionalButton i   -> Additional'Button i
      _                         -> Unknown'Button

-- | Convert GLUTs key states to our internal ones.
instance GLUTConv GLUT.KeyState InputState where
  fromGLUT state =
    case state of
      GLUT.Down       -> Down
      GLUT.Up         -> Up
