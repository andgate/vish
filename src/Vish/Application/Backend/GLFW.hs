{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Vish.Application.Backend.GLFW
  (GLFWState)
where

import Vish.Application.Backend.Types
import Vish.Application.Data.Input (Key (..), MouseButton (..))
import qualified Vish.Application.Data.Input as I

import Control.Concurrent
import Control.Lens
import qualified Control.Exception as X
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.IORef
import Data.Char (toLower)
import Data.List (intercalate)
import Data.Maybe (catMaybes, fromMaybe)
import Text.PrettyPrint
import qualified System.Mem  as System

import qualified Graphics.UI.GLFW          as GLFW
import qualified Graphics.Rendering.OpenGL as GL

-- | State of the GLFW backend library.
data GLFWState
        = GLFWState
        {
        -- | Action that draws on the screen
        _display       :: IO ()
        -- | Window in use by the backend
        , _glfwWindow    :: Maybe GLFW.Window
        }

makeLenses ''GLFWState

-- | Initial GLFW state.
glfwStateInit :: GLFWState
glfwStateInit =
  GLFWState
    { _display       = return ()
    , _glfwWindow    = Nothing
    }

whenWindow :: IORef GLFWState -> (GLFW.Window -> IO a) -> IO (Maybe a)
whenWindow ref f = do
  maybeWindow <- liftM (^. glfwWindow) (readIORef ref)
  forM maybeWindow f

whenWindow_ :: IORef GLFWState -> (GLFW.Window -> IO a) -> IO ()
whenWindow_ ref = void . whenWindow ref

instance Backend GLFWState where
  initBackendState           = glfwStateInit
  initializeBackend          = initializeGLFW
  exitBackend                = exitGLFW
  openWindow                 = openWindowGLFW
  dumpBackendState           = dumpStateGLFW

  installCallbacks ref callbacks =
    mapM_ (\f -> f ref callbacks)
      [ installDisplayCallbackGLFW
      , installWindowFocusCallbackGLFW
      , installWindowCloseCallbackGLFW
      , installReshapeCallbackGLFW
      , installKeyboardCallbackGLFW
      , installMouseMoveCallbackGLFW
      , installMouseButtonCallbackGLFW
      , installScrollCallbackGLFW
      ]

  runMainLoop                = runMainLoopGLFW
  getWindowDimensions ref    = whenWindow ref GLFW.getWindowSize
  --getMousePosition ref       = whenWindow ref GLFW.getCursorPos

  elapsedTime _              = liftM (fromMaybe 0) GLFW.getTime
  sleep _ =
    threadDelay . round . (* 1000000)


-- Initialise -----------------------------------------------------------------
-- | Initialise the GLFW backend.
initializeGLFW :: IORef GLFWState -> Bool-> IO ()
initializeGLFW _ debug = do
  GLFW.init
  glfwVersion <- GLFW.getVersion
  when debug . putStrLn  $ "  glfwVersion        = " ++ show glfwVersion


-- Exit -----------------------------------------------------------------------
-- | Tell the GLFW backend to close the window and exit.
exitGLFW :: IORef GLFWState -> IO ()
exitGLFW ref = do
  glfwState <- readIORef ref
  maybe (return ()) GLFW.destroyWindow (glfwState ^. glfwWindow)
  GLFW.terminate


-- Open Window ----------------------------------------------------------------
-- | Open a new window.
openWindowGLFW :: IORef GLFWState -> Window -> IO ()
openWindowGLFW ref win = do
  maybeMonitor <-
    case win^.winState of
      FullScreen -> GLFW.getPrimaryMonitor
      Windowed   -> return Nothing
  glfwWin <- GLFW.createWindow (win^.winW) (win^.winH) (win^.winName)
                maybeMonitor Nothing

  modifyIORef ref $ glfwWindow .~ glfwWin

  -- Try to enable sync-to-vertical-refresh by setting the number
  -- of buffer swaps per vertical refresh to 1.
  GLFW.swapInterval 1

-- Dump State -----------------------------------------------------------------
-- | Print out the internal GLFW state.
--   This code is taken from a sample in the GLFW repository.
--   TODO: Implement a standard state dump across all backends.
dumpStateGLFW :: IORef GLFWState -> IO ()
dumpStateGLFW ref = do
  maybeWin <- return . (^.glfwWindow) =<< readIORef ref
  case maybeWin of
    Nothing      -> return ()
    Just glfwWin -> do
      version       <- GLFW.getVersion
      versionString <- GLFW.getVersionString
      monitorInfos  <- runMaybeT getMonitorInfos
      joystickNames <- getJoystickNames
      clientAPI     <- GLFW.getWindowClientAPI              glfwWin
      cv0           <- GLFW.getWindowContextVersionMajor    glfwWin
      cv1           <- GLFW.getWindowContextVersionMinor    glfwWin
      cv2           <- GLFW.getWindowContextVersionRevision glfwWin
      robustness    <- GLFW.getWindowContextRobustness      glfwWin
      forwardCompat <- GLFW.getWindowOpenGLForwardCompat    glfwWin
      debug         <- GLFW.getWindowOpenGLDebugContext     glfwWin
      profile       <- GLFW.getWindowOpenGLProfile          glfwWin

      putStrLn $ render $
        nest 4 (
          text "------------------------------------------------------------" $+$
          text "GLFW C library:" $+$
          nest 4 (
            text "Version:"        <+> renderVersion version $+$
            text "Version string:" <+> renderVersionString versionString
          ) $+$
          text "Monitors:" $+$
          nest 4 (
            renderMonitorInfos monitorInfos
          ) $+$
          text "Joysticks:" $+$
          nest 4 (
            renderJoystickNames joystickNames
          ) $+$
          text "OpenGL context:" $+$
          nest 4 (
            text "Client API:"            <+> renderClientAPI clientAPI $+$
            text "Version:"               <+> renderContextVersion cv0 cv1 cv2 $+$
            text "Robustness:"            <+> renderContextRobustness robustness $+$
            text "Forward compatibility:" <+> renderForwardCompat forwardCompat $+$
            text "Debug:"                 <+> renderDebug debug $+$
            text "Profile:"               <+> renderProfile profile
          ) $+$
          text "------------------------------------------------------------"
        )
  where
    renderVersion (GLFW.Version v0 v1 v2) =
        text $ intercalate "." $ map show [v0, v1, v2]

    renderVersionString =
        text . show

    renderMonitorInfos =
        maybe (text "(error)") (vcat . map renderMonitorInfo)

    renderMonitorInfo (name, (x,y), (w,h), vms) =
        text (show name) $+$
        nest 4 (
          location <+> size $+$
          fsep (map renderVideoMode vms)
        )
      where
        location = int x <> text "," <> int y
        size     = int w <> text "x" <> int h <> text "mm"

    renderVideoMode (GLFW.VideoMode w h r g b rr) =
        brackets $ res <+> rgb <+> hz
      where
        res = int w <> text "x" <> int h
        rgb = int r <> text "x" <> int g <> text "x" <> int b
        hz  = int rr <> text "Hz"

    renderJoystickNames pairs =
        vcat $ map (\(js, name) -> text (show js) <+> text (show name)) pairs

    renderContextVersion v0 v1 v2 =
        hcat [int v0, text ".", int v1, text ".", int v2]

    renderClientAPI         = text . show
    renderContextRobustness = text . show
    renderForwardCompat     = text . show
    renderDebug             = text . show
    renderProfile           = text . show


type MonitorInfo = (String, (Int,Int), (Int,Int), [GLFW.VideoMode])

getMonitorInfos :: MaybeT IO [MonitorInfo]
getMonitorInfos =
    getMonitors >>= mapM getMonitorInfo
  where
    getMonitors :: MaybeT IO [GLFW.Monitor]
    getMonitors = MaybeT GLFW.getMonitors

    getMonitorInfo :: GLFW.Monitor -> MaybeT IO MonitorInfo
    getMonitorInfo mon = do
        name <- getMonitorName mon
        vms  <- getVideoModes mon
        MaybeT $ do
            pos  <- liftIO $ GLFW.getMonitorPos mon
            size <- liftIO $ GLFW.getMonitorPhysicalSize mon
            return $ Just (name, pos, size, vms)

    getMonitorName :: GLFW.Monitor -> MaybeT IO String
    getMonitorName mon = MaybeT $ GLFW.getMonitorName mon

    getVideoModes :: GLFW.Monitor -> MaybeT IO [GLFW.VideoMode]
    getVideoModes mon = MaybeT $ GLFW.getVideoModes mon

getJoystickNames :: IO [(GLFW.Joystick, String)]
getJoystickNames =
    catMaybes `fmap` mapM getJoystick joysticks
  where
    getJoystick js =
        fmap (maybe Nothing (\name -> Just (js, name)))
             (GLFW.getJoystickName js)

-- Display Callback -----------------------------------------------------------
-- | Callback for when GLFW needs us to redraw the contents of the window.
installDisplayCallbackGLFW :: IORef GLFWState -> Callbacks -> IO ()
installDisplayCallbackGLFW ref callbacks =
  modifyIORef ref $ display .~ callbackDisplay ref callbacks


callbackDisplay :: IORef GLFWState -> Callbacks -> IO ()
callbackDisplay ref callbacks = do
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  GL.color $ GL.Color4 0 0 0 (1 :: GL.GLfloat)

  displayCallback callbacks ref


-- Focus Callback -------------------------------------------------------------
-- | Callback for when the user is focus/defocused on the window.
--   Used to control pause/resume.
installWindowFocusCallbackGLFW :: IORef GLFWState -> Callbacks -> IO ()
installWindowFocusCallbackGLFW ref callbacks =
  whenWindow_ ref $ \glfwWin ->
    GLFW.setWindowFocusCallback  glfwWin $ Just (callbackWindowFocus ref callbacks)

callbackWindowFocus :: IORef GLFWState -> Callbacks
                    -> GLFW.Window -> GLFW.FocusState -> IO ()
callbackWindowFocus ref callbacks _ focusState =
  case focusState of
    GLFW.FocusState'Focused ->
      resumeCallback callbacks ref
    GLFW.FocusState'Defocused ->
      pauseCallback callbacks ref


-- Close Callback -------------------------------------------------------------
-- | Callback for when the user closes the window.
--   We can do some cleanup here.
installWindowCloseCallbackGLFW :: IORef GLFWState -> Callbacks -> IO ()
installWindowCloseCallbackGLFW ref callbacks =
  whenWindow_ ref $ \glfwWin ->
    GLFW.setWindowCloseCallback glfwWin $ Just (callbackWindowClose ref callbacks)

callbackWindowClose :: IORef GLFWState -> Callbacks -> GLFW.Window -> IO ()
callbackWindowClose ref callbacks _ =
  closeCallback callbacks ref

-- Reshape --------------------------------------------------------------------
-- | Callback for when the user reshapes the window.
installReshapeCallbackGLFW :: IORef GLFWState -> Callbacks -> IO ()
installReshapeCallbackGLFW ref callbacks =
  whenWindow_ ref $ \glfwWin ->
    GLFW.setWindowSizeCallback glfwWin $ Just (callbackReshape ref callbacks)

callbackReshape :: Backend a => IORef a -> Callbacks
                -> GLFW.Window -> Int -> Int -> IO ()
callbackReshape ref callbacks _ =
  reshapeCallback callbacks ref


-- Keyboard Callback ----------------------------------------------------------
installKeyboardCallbackGLFW :: IORef GLFWState -> Callbacks -> IO ()
installKeyboardCallbackGLFW ref callbacks =
  whenWindow_ ref $ \glfwWin ->
    GLFW.setKeyCallback glfwWin $ Just (callbackKeyboard ref callbacks)

callbackKeyboard :: IORef GLFWState -> Callbacks
                  -> GLFW.Window -> GLFW.Key -> Int
                  -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
callbackKeyboard ref callbacks _ key scancode state mods =
  keyboardCallback callbacks ref key' state'
  where
    state' = fromGLFW state
    key' =
      if GLFW.modifierKeysShift mods
        then I.shiftKey . fromGLFW $ key
        else fromGLFW key


-- Mouse Movement Callback ----------------------------------------------------
installMouseMoveCallbackGLFW :: IORef GLFWState -> Callbacks -> IO ()
installMouseMoveCallbackGLFW ref callbacks =
  whenWindow_ ref $ \glfwWin ->
    GLFW.setCursorPosCallback glfwWin $ Just (callbackMouseMove ref callbacks)

callbackMouseMove :: IORef GLFWState -> Callbacks -> GLFW.Window
                -> Double -> Double -> IO ()
callbackMouseMove ref callbacks _ =
  mouseMoveCallback callbacks ref


-- Mouse Button Callback ------------------------------------------------------
installMouseButtonCallbackGLFW :: IORef GLFWState -> Callbacks -> IO ()
installMouseButtonCallbackGLFW ref callbacks =
  whenWindow_ ref $ \glfwWin ->
    GLFW.setMouseButtonCallback glfwWin $ Just (callbackMouseButton ref callbacks)

callbackMouseButton :: IORef GLFWState -> Callbacks
                    -> GLFW.Window -> GLFW.MouseButton -> GLFW.MouseButtonState
                    -> GLFW.ModifierKeys -> IO ()
callbackMouseButton ref callbacks _ button state _ =
  whenWindow_ ref $ \glfwWin -> do
  (posX, posY) <- GLFW.getCursorPos glfwWin
  mouseButtonCallback callbacks ref button' state' posX posY
  where
    button'   = fromGLFW button
    state' = fromGLFW state


-- Scroll Callback ------------------------------------------------------------
installScrollCallbackGLFW :: IORef GLFWState -> Callbacks -> IO ()
installScrollCallbackGLFW ref callbacks =
  whenWindow_ ref $ \glfwWin ->
    GLFW.setScrollCallback glfwWin $ Just (callbackScroll ref callbacks)

callbackScroll :: IORef GLFWState -> Callbacks -> GLFW.Window
                   -> Double -> Double -> IO ()
callbackScroll ref callbacks _ =
  scrollCallback callbacks ref


-- Main Loop ------------------------------------------------------------------
runMainLoopGLFW :: IORef GLFWState -> IO ()
runMainLoopGLFW ref =
  whenWindow_ ref $ \glfwWin -> do
    winShouldClose <- GLFW.windowShouldClose glfwWin
    winFocus <- GLFW.getWindowFocused glfwWin
    if winShouldClose
      then
        exitBackend ref
      else do
        case winFocus of
          GLFW.FocusState'Defocused -> GLFW.waitEvents
          GLFW.FocusState'Focused -> do
            GLFW.pollEvents

            (^.display) =<< readIORef ref
            GLFW.swapBuffers glfwWin
            -- run gc to reduce pauses during mainloop (hopefully)
            System.performGC

        runMainLoopGLFW ref


-- GLFW type Conversion -------------------------------------------------------
class GLFWConv a b where
  fromGLFW :: a -> b

instance GLFWConv GLFW.KeyState InputState where
  fromGLFW keystate =
    case keystate of
      GLFW.KeyState'Pressed   -> Down
      GLFW.KeyState'Repeating -> Down
      GLFW.KeyState'Released  -> Up

instance GLFWConv GLFW.MouseButtonState InputState where
  fromGLFW keystate =
    case keystate of
      GLFW.MouseButtonState'Pressed   -> Down
      GLFW.MouseButtonState'Released  -> Up

instance GLFWConv GLFW.Key I.Key where
  fromGLFW key =
    case key of
      GLFW.Key'Unknown -> Key'Unknown
      GLFW.Key'GraveAccent -> Key'GraveAccent
      GLFW.Key'1 -> Key'1
      GLFW.Key'2 -> Key'2
      GLFW.Key'3 -> Key'3
      GLFW.Key'4 -> Key'4
      GLFW.Key'5 -> Key'5
      GLFW.Key'6 -> Key'6
      GLFW.Key'7 -> Key'7
      GLFW.Key'8 -> Key'8
      GLFW.Key'9 -> Key'9
      GLFW.Key'0 -> Key'0
      GLFW.Key'Minus -> Key'Minus
      GLFW.Key'Equal -> Key'Equal
      GLFW.Key'A -> Key'a
      GLFW.Key'B -> Key'b
      GLFW.Key'C -> Key'c
      GLFW.Key'D -> Key'd
      GLFW.Key'E -> Key'e
      GLFW.Key'F -> Key'f
      GLFW.Key'G -> Key'g
      GLFW.Key'H -> Key'h
      GLFW.Key'I -> Key'i
      GLFW.Key'J -> Key'j
      GLFW.Key'K -> Key'k
      GLFW.Key'L -> Key'l
      GLFW.Key'M -> Key'm
      GLFW.Key'N -> Key'n
      GLFW.Key'O -> Key'o
      GLFW.Key'P -> Key'p
      GLFW.Key'Q -> Key'q
      GLFW.Key'R -> Key'r
      GLFW.Key'S -> Key's
      GLFW.Key'T -> Key't
      GLFW.Key'U -> Key'u
      GLFW.Key'V -> Key'v
      GLFW.Key'W -> Key'w
      GLFW.Key'X -> Key'x
      GLFW.Key'Y -> Key'y
      GLFW.Key'Z -> Key'z
      GLFW.Key'LeftBracket -> Key'LeftBracket
      GLFW.Key'RightBracket -> Key'RightBracket
      GLFW.Key'Backslash -> Key'Backslash
      GLFW.Key'Semicolon -> Key'Semicolon
      GLFW.Key'Apostrophe -> Key'Apostrophe
      GLFW.Key'Comma -> Key'Comma
      GLFW.Key'Period -> Key'Period
      GLFW.Key'Slash -> Key'Slash
      GLFW.Key'World1 -> Key'World1
      GLFW.Key'World2 -> Key'World2
      GLFW.Key'Space       -> Key'Space
      GLFW.Key'Escape      -> Key'Escape
      GLFW.Key'Enter       -> Key'Enter
      GLFW.Key'Tab         -> Key'Tab
      GLFW.Key'Backspace   -> Key'Backspace
      GLFW.Key'Insert      -> Key'Insert
      GLFW.Key'Delete      -> Key'Delete
      GLFW.Key'Right       -> Key'Right
      GLFW.Key'Left        -> Key'Left
      GLFW.Key'Up          -> Key'Up
      GLFW.Key'Down        -> Key'Down
      GLFW.Key'PageUp      -> Key'PageUp
      GLFW.Key'PageDown    -> Key'PageDown
      GLFW.Key'Home        -> Key'Home
      GLFW.Key'End         -> Key'End
      GLFW.Key'CapsLock -> Key'CapsLock
      GLFW.Key'ScrollLock -> Key'ScrollLock
      GLFW.Key'NumLock -> Key'NumLock
      GLFW.Key'PrintScreen -> Key'PrintScreen
      GLFW.Key'Pause -> Key'Pause
      GLFW.Key'F1          -> Key'F1
      GLFW.Key'F2          -> Key'F2
      GLFW.Key'F3          -> Key'F3
      GLFW.Key'F4          -> Key'F4
      GLFW.Key'F5          -> Key'F5
      GLFW.Key'F6          -> Key'F6
      GLFW.Key'F7          -> Key'F7
      GLFW.Key'F8          -> Key'F8
      GLFW.Key'F9          -> Key'F9
      GLFW.Key'F10         -> Key'F10
      GLFW.Key'F11         -> Key'F11
      GLFW.Key'F12         -> Key'F12
      GLFW.Key'F13         -> Key'F13
      GLFW.Key'F14         -> Key'F14
      GLFW.Key'F15         -> Key'F15
      GLFW.Key'F16         -> Key'F16
      GLFW.Key'F17         -> Key'F17
      GLFW.Key'F18         -> Key'F18
      GLFW.Key'F19         -> Key'F19
      GLFW.Key'F20         -> Key'F20
      GLFW.Key'F21         -> Key'F21
      GLFW.Key'F22         -> Key'F22
      GLFW.Key'F23         -> Key'F23
      GLFW.Key'F24         -> Key'F24
      GLFW.Key'F25         -> Key'F25
      GLFW.Key'Pad0        -> Key'Pad0
      GLFW.Key'Pad1        -> Key'Pad1
      GLFW.Key'Pad2        -> Key'Pad2
      GLFW.Key'Pad3        -> Key'Pad3
      GLFW.Key'Pad4        -> Key'Pad4
      GLFW.Key'Pad5        -> Key'Pad5
      GLFW.Key'Pad6        -> Key'Pad6
      GLFW.Key'Pad7        -> Key'Pad7
      GLFW.Key'Pad8        -> Key'Pad8
      GLFW.Key'Pad9        -> Key'Pad9
      GLFW.Key'PadDecimal  -> Key'PadDecimal
      GLFW.Key'PadDivide   -> Key'PadDivide
      GLFW.Key'PadMultiply -> Key'PadMultiply
      GLFW.Key'PadSubtract -> Key'PadSubtract
      GLFW.Key'PadAdd      -> Key'PadAdd
      GLFW.Key'PadEnter    -> Key'PadEnter
      GLFW.Key'PadEqual    -> Key'PadEqual
      GLFW.Key'LeftShift -> Key'LeftShift
      GLFW.Key'LeftControl -> Key'LeftControl
      GLFW.Key'LeftAlt -> Key'LeftAlt
      GLFW.Key'LeftSuper -> Key'LeftSuper
      GLFW.Key'RightShift -> Key'RightShift
      GLFW.Key'RightControl -> Key'RightControl
      GLFW.Key'RightAlt -> Key'RightAlt
      GLFW.Key'RightSuper -> Key'RightSuper
      GLFW.Key'Menu -> Key'Menu

instance GLFWConv GLFW.MouseButton MouseButton where
  fromGLFW mouse
   = case mouse of
        GLFW.MouseButton'1 -> Left'Button
        GLFW.MouseButton'2 -> Right'Button
        GLFW.MouseButton'3 -> Middle'Button
        GLFW.MouseButton'4 -> Additional'Button 3
        GLFW.MouseButton'5 -> Additional'Button 4
        GLFW.MouseButton'6 -> Additional'Button 5
        GLFW.MouseButton'7 -> Additional'Button 6
        GLFW.MouseButton'8 -> Additional'Button 7


joysticks :: [GLFW.Joystick]
joysticks =
  [ GLFW.Joystick'1
  , GLFW.Joystick'2
  , GLFW.Joystick'3
  , GLFW.Joystick'4
  , GLFW.Joystick'5
  , GLFW.Joystick'6
  , GLFW.Joystick'7
  , GLFW.Joystick'8
  , GLFW.Joystick'9
  , GLFW.Joystick'10
  , GLFW.Joystick'11
  , GLFW.Joystick'12
  , GLFW.Joystick'13
  , GLFW.Joystick'14
  , GLFW.Joystick'15
  , GLFW.Joystick'16
  ]
