{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Vish.Application.Backend.GLFW
  (GLFWState)
where

import Vish.Application.Backend.Types

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
        { -- | Status of Ctrl, Alt or Shift (Up or Down?)
          _modifiers     :: Modifiers

        -- | Does the screen need to be redrawn?
        , _dirtyScreen   :: Bool

        -- | Action that draws on the screen
        , _display       :: IO ()

        -- | Action perforrmed when idling
        , _idle          :: IO ()

        , _glfwWindow    :: Maybe GLFW.Window
        }

makeLenses ''GLFWState

-- | Initial GLFW state.
glfwStateInit :: GLFWState
glfwStateInit =
  GLFWState
    { _modifiers     = Modifiers Up Up Up
    , _dirtyScreen   = True
    , _display       = return ()
    , _idle          = return ()
    , _glfwWindow    = Nothing
    }

whenWindow :: IORef GLFWState -> (GLFW.Window -> IO ()) -> IO ()
whenWindow ref f = do
  maybeWindow <- liftM (^. glfwWindow) (readIORef ref)
  forM_ maybeWindow f


instance Backend GLFWState where
  initBackendState           = glfwStateInit
  initializeBackend          = initializeGLFW
  exitBackend                = exitGLFW
  openWindow                 = openWindowGLFW
  dumpBackendState           = dumpStateGLFW

  installCallbacks ref callbacks =
    mapM_ (\f -> f ref callbacks)
      [ installDisplayCallbackGLFW
      , installIdleCallbackGLFW
      , installWindowCloseCallbackGLFW
      , installReshapeCallbackGLFW
      , installKeyboardCallbackGLFW
      , installMouseMoveCallbackGLFW
      , installMouseButtonCallbackGLFW
      , installScrollCallbackGLFW
      ]

  runMainLoop                = runMainLoopGLFW
  postRedisplay              = postRedisplayGLFW
  getWindowDimensions        = getWindowDimensionsGLFW

  elapsedTime                = elapsedTimeGLFW
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


-- Idle Callback --------------------------------------------------------------
-- | Callback for when GLFW has finished its jobs and it's time for us to do
--   something for our application.
installIdleCallbackGLFW :: IORef GLFWState -> Callbacks -> IO ()
installIdleCallbackGLFW ref callbacks =
  modifyIORef ref $ idle .~ idleCallback callbacks ref


-- Close Callback -------------------------------------------------------------
-- | Callback for when the user closes the window.
--   We can do some cleanup here.
installWindowCloseCallbackGLFW :: IORef GLFWState -> Callbacks -> IO ()
installWindowCloseCallbackGLFW ref callbacks =
  whenWindow ref $ \glfwWin ->
    GLFW.setWindowCloseCallback glfwWin $ Just (windowCloseCallback ref callbacks)

windowCloseCallback :: IORef GLFWState -> Callbacks -> GLFW.Window -> IO ()
windowCloseCallback ref callbacks _ =
  closeCallback callbacks ref

-- Reshape --------------------------------------------------------------------
-- | Callback for when the user reshapes the window.
installReshapeCallbackGLFW :: IORef GLFWState -> Callbacks -> IO ()
installReshapeCallbackGLFW ref callbacks =
  whenWindow ref $ \glfwWin ->
    GLFW.setWindowSizeCallback glfwWin $ Just (callbackReshape ref callbacks)

callbackReshape :: Backend a => IORef a -> Callbacks
                -> GLFW.Window -> Int -> Int -> IO ()
callbackReshape ref callbacks _ sizeX sizeY =
  reshapeCallback callbacks ref (sizeX, sizeY)


-- Keyboard Callback ----------------------------------------------------------
installKeyboardCallbackGLFW :: IORef GLFWState -> Callbacks -> IO ()
installKeyboardCallbackGLFW ref callbacks =
  whenWindow ref $ \glfwWin ->
    GLFW.setKeyCallback glfwWin $ Just (callbackKeyboard ref callbacks)

callbackKeyboard :: IORef GLFWState -> Callbacks
                  -> GLFW.Window -> GLFW.Key -> Int
                  -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
callbackKeyboard ref callbacks _ key scancode keystate mods = do
  let key'      = fromGLFW key
      keystate' = fromGLFW keystate
      mods'     = fromGLFW mods

  modifyIORef ref $ modifiers .~ mods'
  keyboardCallback callbacks ref key' keystate' mods'


-- Mouse Movement Callback ----------------------------------------------------
-- | Callback for when the user moves the mouse.
installMouseMoveCallbackGLFW :: IORef GLFWState -> Callbacks -> IO ()
installMouseMoveCallbackGLFW ref callbacks =
  whenWindow ref $ \glfwWin ->
    GLFW.setCursorPosCallback glfwWin $ Just (callbackMouseMove ref callbacks)

callbackMouseMove :: IORef GLFWState -> Callbacks -> GLFW.Window
                -> Double -> Double -> IO ()
callbackMouseMove ref callbacks _ =
  mouseMoveCallback callbacks ref


-- Mouse Button Callback ------------------------------------------------------
installMouseButtonCallbackGLFW :: IORef GLFWState -> Callbacks -> IO ()
installMouseButtonCallbackGLFW ref callbacks =
  whenWindow ref $ \glfwWin ->
    GLFW.setMouseButtonCallback glfwWin $ Just (callbackMouseButton ref callbacks)

callbackMouseButton :: IORef GLFWState -> Callbacks -> GLFW.Window
                    -> GLFW.MouseButton -> GLFW.MouseButtonState
                    -> GLFW.ModifierKeys -> IO ()
callbackMouseButton ref callbacks _ button keystate mods = do
  let button'   = fromGLFW button
      keystate' = fromGLFW keystate
      mods'     = fromGLFW mods
  mouseButtonCallback callbacks ref button' keystate' mods'


-- Scroll Callback ------------------------------------------------------------
installScrollCallbackGLFW :: IORef GLFWState -> Callbacks -> IO ()
installScrollCallbackGLFW ref callbacks =
  whenWindow ref $ \glfwWin ->
    GLFW.setScrollCallback glfwWin $ Just (callbackScroll ref callbacks)

callbackScroll :: IORef GLFWState -> Callbacks -> GLFW.Window
                   -> Double -> Double -> IO ()
callbackScroll ref callbacks _ =
  scrollCallback callbacks ref


-- Main Loop ------------------------------------------------------------------
runMainLoopGLFW :: IORef GLFWState -> IO ()
runMainLoopGLFW ref =
  X.catch go exit
  where
    exit :: X.SomeException -> IO ()
    exit e = print e >> exitGLFW ref

    go :: IO ()
    go =
      whenWindow ref $ \glfwWin -> do
        winShouldClose <- GLFW.windowShouldClose glfwWin
        unless winShouldClose $ do
          GLFW.pollEvents
          dirty <- return . (^.dirtyScreen) =<< readIORef ref

          when dirty $ do
            (^.display) =<< readIORef ref
            GLFW.swapBuffers glfwWin

          modifyIORef ref $ dirtyScreen .~ False
          (^.idle) =<< readIORef ref
          -- run gc to reduce pauses during mainloop (hopefully)
          System.performGC
          runMainLoopGLFW ref


-- Redisplay ------------------------------------------------------------------
postRedisplayGLFW :: IORef GLFWState -> IO ()
postRedisplayGLFW stateRef =
  modifyIORef stateRef $ dirtyScreen .~ True


-- Get window dimensions ------------------------------------------------------
getWindowDimensionsGLFW :: IORef GLFWState -> IO (Maybe (Int, Int))
getWindowDimensionsGLFW ref = do
  maybeWindow <- liftM (^. glfwWindow) (readIORef ref)
  case maybeWindow of
    Nothing -> return Nothing
    Just glfwWin -> do
      size <- GLFW.getWindowSize glfwWin
      return . Just $ size


-- Get the elapsed time -------------------------------------------------------
elapsedTimeGLFW :: IORef GLFWState -> IO Double
elapsedTimeGLFW _ = do
  maybeTime <- GLFW.getTime
  return $ fromMaybe 0 maybeTime

-- Key Code Conversion --------------------------------------------------------
class GLFWConv a b where
  fromGLFW :: a -> b

instance GLFWConv GLFW.KeyState KeyState where
  fromGLFW keystate =
    case keystate of
      GLFW.KeyState'Pressed   -> Down
      GLFW.KeyState'Repeating -> Down
      GLFW.KeyState'Released  -> Up

instance GLFWConv GLFW.MouseButtonState KeyState where
  fromGLFW keystate =
    case keystate of
      GLFW.MouseButtonState'Pressed   -> Down
      GLFW.MouseButtonState'Released  -> Up

instance GLFWConv GLFW.ModifierKeys Modifiers where
  fromGLFW (GLFW.ModifierKeys s c a _) =
    Modifiers {
      _shift = if s then Down else Up,
      _ctrl  = if c then Down else Up,
      _alt   = if a then Down else Up
    }

instance GLFWConv GLFW.Key Key where
  fromGLFW key =
    case key of
      GLFW.Key'Unknown -> KeyUnknown
      GLFW.Key'Apostrophe -> KeyApostrophe
      GLFW.Key'Comma -> KeyComma
      GLFW.Key'Minus -> KeyMinus
      GLFW.Key'Period -> KeyPeriod
      GLFW.Key'Slash -> KeySlash
      GLFW.Key'0 -> Key0
      GLFW.Key'1 -> Key1
      GLFW.Key'2 -> Key2
      GLFW.Key'3 -> Key3
      GLFW.Key'4 -> Key4
      GLFW.Key'5 -> Key5
      GLFW.Key'6 -> Key6
      GLFW.Key'7 -> Key7
      GLFW.Key'8 -> Key8
      GLFW.Key'9 -> Key9
      GLFW.Key'Semicolon -> KeySemicolon
      GLFW.Key'Equal -> KeyEqual
      GLFW.Key'A -> KeyA
      GLFW.Key'B -> KeyB
      GLFW.Key'C -> KeyC
      GLFW.Key'D -> KeyD
      GLFW.Key'E -> KeyE
      GLFW.Key'F -> KeyF
      GLFW.Key'G -> KeyG
      GLFW.Key'H -> KeyH
      GLFW.Key'I -> KeyI
      GLFW.Key'J -> KeyJ
      GLFW.Key'K -> KeyK
      GLFW.Key'L -> KeyL
      GLFW.Key'M -> KeyM
      GLFW.Key'N -> KeyN
      GLFW.Key'O -> KeyO
      GLFW.Key'P -> KeyP
      GLFW.Key'Q -> KeyQ
      GLFW.Key'R -> KeyR
      GLFW.Key'S -> KeyS
      GLFW.Key'T -> KeyT
      GLFW.Key'U -> KeyU
      GLFW.Key'V -> KeyV
      GLFW.Key'W -> KeyW
      GLFW.Key'X -> KeyX
      GLFW.Key'Y -> KeyY
      GLFW.Key'Z -> KeyZ
      GLFW.Key'LeftBracket -> KeyLeftBracket
      GLFW.Key'Backslash -> KeyBackslash
      GLFW.Key'RightBracket -> KeyRightBracket
      GLFW.Key'GraveAccent -> KeyGraveAccent
      GLFW.Key'World1 -> KeyWorld1
      GLFW.Key'World2 -> KeyWorld2
      GLFW.Key'Space       -> KeySpace
      GLFW.Key'Escape      -> KeyEscape
      GLFW.Key'Enter       -> KeyEnter
      GLFW.Key'Tab         -> KeyTab
      GLFW.Key'Backspace   -> KeyBackspace
      GLFW.Key'Insert      -> KeyInsert
      GLFW.Key'Delete      -> KeyDelete
      GLFW.Key'Right       -> KeyRight
      GLFW.Key'Left        -> KeyLeft
      GLFW.Key'Up          -> KeyUp
      GLFW.Key'Down        -> KeyDown
      GLFW.Key'PageUp      -> KeyPageUp
      GLFW.Key'PageDown    -> KeyPageDown
      GLFW.Key'Home        -> KeyHome
      GLFW.Key'End         -> KeyEnd
      GLFW.Key'CapsLock -> KeyCapsLock
      GLFW.Key'ScrollLock -> KeyScrollLock
      GLFW.Key'NumLock -> KeyNumLock
      GLFW.Key'PrintScreen -> KeyPrintScreen
      GLFW.Key'Pause -> KeyPause
      GLFW.Key'F1          -> KeyF1
      GLFW.Key'F2          -> KeyF2
      GLFW.Key'F3          -> KeyF3
      GLFW.Key'F4          -> KeyF4
      GLFW.Key'F5          -> KeyF5
      GLFW.Key'F6          -> KeyF6
      GLFW.Key'F7          -> KeyF7
      GLFW.Key'F8          -> KeyF8
      GLFW.Key'F9          -> KeyF9
      GLFW.Key'F10         -> KeyF10
      GLFW.Key'F11         -> KeyF11
      GLFW.Key'F12         -> KeyF12
      GLFW.Key'F13         -> KeyF13
      GLFW.Key'F14         -> KeyF14
      GLFW.Key'F15         -> KeyF15
      GLFW.Key'F16         -> KeyF16
      GLFW.Key'F17         -> KeyF17
      GLFW.Key'F18         -> KeyF18
      GLFW.Key'F19         -> KeyF19
      GLFW.Key'F20         -> KeyF20
      GLFW.Key'F21         -> KeyF21
      GLFW.Key'F22         -> KeyF22
      GLFW.Key'F23         -> KeyF23
      GLFW.Key'F24         -> KeyF24
      GLFW.Key'F25         -> KeyF25
      GLFW.Key'Pad0        -> KeyPad0
      GLFW.Key'Pad1        -> KeyPad1
      GLFW.Key'Pad2        -> KeyPad2
      GLFW.Key'Pad3        -> KeyPad3
      GLFW.Key'Pad4        -> KeyPad4
      GLFW.Key'Pad5        -> KeyPad5
      GLFW.Key'Pad6        -> KeyPad6
      GLFW.Key'Pad7        -> KeyPad7
      GLFW.Key'Pad8        -> KeyPad8
      GLFW.Key'Pad9        -> KeyPad9
      GLFW.Key'PadDecimal  -> KeyPadDecimal
      GLFW.Key'PadDivide   -> KeyPadDivide
      GLFW.Key'PadMultiply -> KeyPadMultiply
      GLFW.Key'PadSubtract -> KeyPadSubtract
      GLFW.Key'PadAdd      -> KeyPadAdd
      GLFW.Key'PadEnter    -> KeyPadEnter
      GLFW.Key'PadEqual    -> KeyPadEqual
      GLFW.Key'LeftShift -> KeyLeftShift
      GLFW.Key'LeftControl -> KeyLeftControl
      GLFW.Key'LeftAlt -> KeyLeftAlt
      GLFW.Key'LeftSuper -> KeyLeftSuper
      GLFW.Key'RightShift -> KeyRightShift
      GLFW.Key'RightControl -> KeyRightControl
      GLFW.Key'RightAlt -> KeyRightAlt
      GLFW.Key'RightSuper -> KeyRightSuper
      GLFW.Key'Menu -> KeyMenu

instance GLFWConv GLFW.MouseButton MouseButton where
  fromGLFW mouse
   = case mouse of
        GLFW.MouseButton'1 -> LeftButton
        GLFW.MouseButton'2 -> RightButton
        GLFW.MouseButton'3 -> MiddleButton
        GLFW.MouseButton'4 -> AdditionalButton 3
        GLFW.MouseButton'5 -> AdditionalButton 4
        GLFW.MouseButton'6 -> AdditionalButton 5
        GLFW.MouseButton'7 -> AdditionalButton 6
        GLFW.MouseButton'8 -> AdditionalButton 7


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
