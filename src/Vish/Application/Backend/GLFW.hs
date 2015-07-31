{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE CPP #-}
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
import Data.Maybe (catMaybes)
import Text.PrettyPrint
import qualified System.Mem  as System

--import Graphics.UI.GLFW                    (WindowValue(..))
import qualified Graphics.UI.GLFW          as GLFW
import qualified Graphics.Rendering.OpenGL as GL

-- [Note: FreeGlut]
-- ~~~~~~~~~~~~~~~~
-- We use GLUT for font rendering.
--   On freeglut-based installations (usually linux) we need to explicitly
--   initialize GLUT before we can use any of it's functions.
--
---  We also need to deinitialize (exit) GLUT when we close the GLFW
--   window, otherwise opening a gloss window again from GHCi will crash.
--   For the OS X and Windows version of GLUT there are no such restrictions.
--
--   We assume also assume that only linux installations use freeglut.
--
#ifdef linux_HOST_OS
import qualified Graphics.UI.GLUT          as GLUT
#endif

-- | State of the GLFW backend library.
data GLFWState
        = GLFWState
        { -- | Status of Ctrl, Alt or Shift (Up or Down?)
          _modifiers     :: Modifiers

        -- | Latest mouse position
        , _mousePosition :: (Int,Int)

        -- | Latest mousewheel position
        , _mouseWheelPos :: (Int, Int)

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
    { modifiers     = Modifiers Up Up Up
    , mousePosition = (0, 0)
    , mouseWheelPos = (0, 0)
    , dirtyScreen   = True
    , display       = return ()
    , idle          = return ()
    , glfwWindow    = Nothing
    }


instance Backend GLFWState where
  initBackendState           = glfwStateInit
  initializeBackend          = initializeGLFW
  exitBackend                = exitGLFW
  openWindow                 = openWindowGLFW
  dumpBackendState           = dumpStateGLFW
  installDisplayCallback     = installDisplayCallbackGLFW
  installWindowCloseCallback = installWindowCloseCallbackGLFW
  installReshapeCallback     = installReshapeCallbackGLFW
  installKeyMouseCallback    = installKeyMouseCallbackGLFW
  installMotionCallback      = installMotionCallbackGLFW
  installIdleCallback        = installIdleCallbackGLFW
  runMainLoop                = runMainLoopGLFW
  postRedisplay              = postRedisplayGLFW
  getWindowDimensions _ =
    GLFW.getWindowSize
  elapsedTime _ =
    GLFW.getTime
  sleep _ =
    threadDelay . round . (* 1000000)


-- Initialise -----------------------------------------------------------------
-- | Initialise the GLFW backend.
initializeGLFW :: IORef GLFWState -> Bool-> IO ()
initializeGLFW _ debug = do
  GLFW.init
  glfwVersion <- GLFW.getVersion

#ifdef linux_HOST_OS
-- See [Note: FreeGlut] for why we need this.
  (_progName, _args)  <- GLUT.getArgsAndInitialize
#endif

  when debug . putStrLn  $ "  glfwVersion        = " ++ show glfwVersion


-- Exit -----------------------------------------------------------------------
-- | Tell the GLFW backend to close the window and exit.
exitGLFW :: IORef GLFWState -> IO ()
exitGLFW ref = do
#ifdef linux_HOST_OS
-- See [Note: FreeGlut] on why we exit GLUT for Linux
  GLUT.exit
#endif
  glfwState <- readIORef ref
  maybe (glfwWindow glfwState) (return ()) GLFW.destroyWindow
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

  modifyIORef ref $ glfwWindow %~ glfwWin

  -- Try to enable sync-to-vertical-refresh by setting the number
  -- of buffer swaps per vertical refresh to 1.
  GLFW.swapInterval 1

-- Dump State -----------------------------------------------------------------
-- | Print out the internal GLFW state.
dumpStateGLFW :: IORef a -> IO ()
dumpStateGLFW ref = do
  glfwWin <- readIORef ref
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
  modifyIORef ref $ display %~ displayCallback callbacks


callbackDisplay :: IORef GLFWState -> Callbacks -> IO ()
callbackDisplay ref callbacks = do
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  GL.color $ GL.Color4 0 0 0 (1 :: GL.GLfloat)

  displayCallback callbacks ref


-- Close Callback -------------------------------------------------------------
-- | Callback for when the user closes the window.
--   We can do some cleanup here.
installWindowCloseCallbackGLFW :: IORef GLFWState -> Callbacks -> IO ()
installWindowCloseCallbackGLFW ref callbacks =
  GLFW.setWindowCloseCallback $ do
    closeCallback callbacks ref
#ifdef linux_HOST_OS
-- See [Note: FreeGlut] for why we need this.
    GLUT.exit
#endif
    return True


-- Reshape --------------------------------------------------------------------
-- | Callback for when the user reshapes the window.
installReshapeCallbackGLFW :: Backend a => IORef a -> Callbacks -> IO ()
installReshapeCallbackGLFW stateRef callbacks =
  GLFW.setWindowSizeCallback (callbackReshape stateRef callbacks)

callbackReshape :: Backend a => IORef a -> Callbacks -> Int -> Int -> IO ()
callbackReshape ref callbacks sizeX sizeY =
  reshapeCallback callbacks ref (sizeX, sizeY)

-- KeyMouse -----------------------------------------------------------------------
-- | Callbacks for when the user presses a key or moves / clicks the mouse.
--   This is a bit verbose because we have to do impedence matching between
--   GLFW's event system, and the one use by Gloss which was originally
--   based on GLUT. The main problem is that GLUT only provides a single callback
--   slot for character keys, arrow keys, mouse buttons and mouse wheel movement,
--   while GLFW provides a single slot for each.
--
installKeyMouseCallbackGLFW :: IORef GLFWState -> Callbacks -> IO ()
installKeyMouseCallbackGLFW stateRef callbacks = do
  GLFW.setKeyCallback         $ callbackKeyboard    stateRef callbacks
  GLFW.setCharCallback        $ callbackChar        stateRef callbacks
  GLFW.setMouseButtonCallback $ callbackMouseButton stateRef callbacks
  GLFW.setScrollCallback      $ callbackMouseWheel  stateRef callbacks


-- GLFW calls this on a non-character keyboard action.
callbackKeyboard :: IORef GLFWState -> Callbacks -> GLFW.Key -> Bool -> IO ()
callbackKeyboard ref callbacks key keystate = do
  let key'      = fromGLFW key
      keystate' = if keystate then Down else Up
      isCharKey (Char _) = True
      isCharKey _        = False

  modsSet <- setModifiers ref key keystate
  (mods, pos) <- readIORef ref >>= \s -> (s^.modifiers, s^.mousePosition)
  unless (modsSet || isCharKey key' && keystate)
    $ keyboardMouseCallback callbacks ref key' keystate' mods pos

setModifiers :: IORef GLFWState -> GLFW.Key -> Bool -> IO Bool
setModifiers ref key pressed = do
  mods <- (modifiers^.) =<< readIORef ref
  let mods' =
        case key of
          GLFW.Key'LeftShift    -> mods {shift = if pressed then Down else Up}
          GLFW.Key'LeftControl  -> mods {ctrl  = if pressed then Down else Up}
          GLFW.Key'LeftAlt      -> mods {alt   = if pressed then Down else Up}
          _                     -> mods

  if mods' /= mods
    then do
          modifyIORef ref $ modifiers %~ mods
          return True
    else return False


-- GLFW calls this on a when the user presses or releases a character key.
callbackChar :: IORef GLFWState -> Callbacks -> Char -> Bool -> IO ()
callbackChar ref callbacks char keystate = do
  let key'      = charToSpecial char
  let keystate' = if keystate then Down else Up

  (mods, pos) <- readIORef ref >>= \s -> (s^.modifiers, s^.mousePosition)
  keyboardMouseCallback callbacks ref key' keystate' mods pos


-- GLFW calls on this when the user clicks or releases a mouse button.
callbackMouseButton :: IORef GLFWState -> Callbacks -> GLFW.MouseButton -> Bool -> IO ()
callbackMouseButton ref callbacks key keystate = do
  let key'      = fromGLFW key
  let keystate' = if keystate then Down else Up

  (mods, pos) <- readIORef ref >>= \s -> (s^.modifiers, s^.mousePosition)
  keyboardMouseCallback callbacks ref key' keystate' mods pos


-- GLFW calls on this when the user moves the mouse wheel.
callbackMouseWheel :: IORef GLFWState -> Callbacks -> Int -> IO ()
callbackMouseWheel ref callbacks w = do
  (key, keystate)  <- setMouseWheel ref w
  (mods, pos) <- readIORef ref >>= \s -> (s^.modifiers, s^.mousePosition)
  keyboardMouseCallback callbacks ref key keystate mods pos

setMouseWheel :: IORef GLFWState -> Int -> Int -> IO (Key, KeyState)
setMouseWheel ref x y = do
  glfwState <- readIORef ref
  modifyIORef ref $ mouseWheelPos %~ (x,y)
  case compare (x,y) (_mouseWheelPos glfwState) of
          LT -> return (MouseButton WheelDown , Down)
          GT -> return (MouseButton WheelUp   , Down)
          EQ -> return (SpecialKey  KeyUnknown, Up  )


-- Motion Callback ------------------------------------------------------------
-- | Callback for when the user moves the mouse.
installMotionCallbackGLFW :: IORef GLFWState -> Callbacks -> IO ()
installMotionCallbackGLFW ref callbacks =
  GLFW.setCursorPosCallback $ callbackMotion ref callbacks

callbackMotion :: IORef GLFWState -> Callbacks -> Int -> Int -> IO ()
callbackMotion ref callbacks x y = do
  modifyIORef ref $ mousePosition %~ (x,y)
  motionCallback callbacks (x,y)


-- Idle Callback --------------------------------------------------------------
-- | Callback for when GLFW has finished its jobs and it's time for us to do
--   something for our application.
installIdleCallbackGLFW :: IORef GLFWState -> Callbacks -> IO ()
installIdleCallbackGLFW ref callbacks =
  modifyIORef ref $ idle %~ idleCallback callbacks ref


-- Main Loop ------------------------------------------------------------------
runMainLoopGLFW :: IORef GLFWState -> IO ()
runMainLoopGLFW stateRef =
  X.catch go exit
  where
    exit :: X.SomeException -> IO ()
    exit e = print e >> exitGLFW stateRef

    go :: IO ()
    go = do
      windowIsOpen <- GLFW.windowIsOpen
      when windowIsOpen $ do
        GLFW.pollEvents
        dirty <- dirtyScreen <$> readIORef stateRef

        when dirty
          $ do   s <- readIORef stateRef
                 display s
                 GLFW.swapBuffers

        modifyIORef stateRef $ \s -> s { dirtyScreen = False }
        readIORef stateRef >>= \s -> idle s
        -- run gc to reduce pauses during mainloop (hopefully)
        System.performGC
        runMainLoopGLFW stateRef


-- Redisplay ------------------------------------------------------------------
postRedisplayGLFW :: IORef GLFWState -> IO ()
postRedisplayGLFW stateRef =
  modifyIORef stateRef $ dirtyScreen %~ True


-- Key Code Conversion --------------------------------------------------------
class GLFWKey a where
  fromGLFW :: a -> Key

instance GLFWKey GLFW.Key where
  fromGLFW key
   = case key of
        GLFW.CharKey c      -> charToSpecial (toLower c)
        GLFW.Key'Space       -> SpecialKey KeySpace
        GLFW.Key'Escape      -> SpecialKey KeyEsc
        GLFW.Key'F1          -> SpecialKey KeyF1
        GLFW.Key'F2          -> SpecialKey KeyF2
        GLFW.Key'F3          -> SpecialKey KeyF3
        GLFW.Key'F4          -> SpecialKey KeyF4
        GLFW.Key'F5          -> SpecialKey KeyF5
        GLFW.Key'F6          -> SpecialKey KeyF6
        GLFW.Key'F7          -> SpecialKey KeyF7
        GLFW.Key'F8          -> SpecialKey KeyF8
        GLFW.Key'F9          -> SpecialKey KeyF9
        GLFW.Key'F10         -> SpecialKey KeyF10
        GLFW.Key'F11         -> SpecialKey KeyF11
        GLFW.Key'F12         -> SpecialKey KeyF12
        GLFW.Key'F13         -> SpecialKey KeyF13
        GLFW.Key'F14         -> SpecialKey KeyF14
        GLFW.Key'F15         -> SpecialKey KeyF15
        GLFW.Key'F16         -> SpecialKey KeyF16
        GLFW.Key'F17         -> SpecialKey KeyF17
        GLFW.Key'F18         -> SpecialKey KeyF18
        GLFW.Key'F19         -> SpecialKey KeyF19
        GLFW.Key'F20         -> SpecialKey KeyF20
        GLFW.Key'F21         -> SpecialKey KeyF21
        GLFW.Key'F22         -> SpecialKey KeyF22
        GLFW.Key'F23         -> SpecialKey KeyF23
        GLFW.Key'F24         -> SpecialKey KeyF24
        GLFW.Key'F25         -> SpecialKey KeyF25
        GLFW.Key'Up          -> SpecialKey KeyUp
        GLFW.Key'Down        -> SpecialKey KeyDown
        GLFW.Key'Left        -> SpecialKey KeyLeft
        GLFW.Key'Right       -> SpecialKey KeyRight
        GLFW.Key'Tab         -> SpecialKey KeyTab
        GLFW.Key'Enter       -> SpecialKey KeyEnter
        GLFW.Key'Backspace   -> SpecialKey KeyBackspace
        GLFW.Key'Insert      -> SpecialKey KeyInsert
        GLFW.Key'Del         -> SpecialKey KeyDelete
        GLFW.Key'Pageup      -> SpecialKey KeyPageUp
        GLFW.Key'Pagedown    -> SpecialKey KeyPageDown
        GLFW.Key'Home        -> SpecialKey KeyHome
        GLFW.Key'End         -> SpecialKey KeyEnd
        GLFW.Key'Pad0        -> SpecialKey KeyPad0
        GLFW.Key'Pad1        -> SpecialKey KeyPad1
        GLFW.Key'Pad2        -> SpecialKey KeyPad2
        GLFW.Key'Pad3        -> SpecialKey KeyPad3
        GLFW.Key'Pad4        -> SpecialKey KeyPad4
        GLFW.Key'Pad5        -> SpecialKey KeyPad5
        GLFW.Key'Pad6        -> SpecialKey KeyPad6
        GLFW.Key'Pad7        -> SpecialKey KeyPad7
        GLFW.Key'Pad8        -> SpecialKey KeyPad8
        GLFW.Key'Pad9        -> SpecialKey KeyPad9
        GLFW.Key'PadDivide   -> SpecialKey KeyPadDivide
        GLFW.Key'PadMultiply -> SpecialKey KeyPadMultiply
        GLFW.Key'PadSubtract -> SpecialKey KeyPadSubtract
        GLFW.Key'PadAdd      -> SpecialKey KeyPadAdd
        GLFW.Key'PadDecimal  -> SpecialKey KeyPadDecimal
        GLFW.Key'PadEqual    -> Char '='
        GLFW.Key'PadEnter    -> SpecialKey KeyPadEnter
        _                   -> SpecialKey KeyUnknown


-- | Convert char keys to special keys to work around a bug in
--   GLFW 2.7. On OS X, GLFW sometimes registers special keys as char keys,
--   so we convert them back here.
--   GLFW 2.7 is current as of Nov 2011, and is shipped with the Hackage
--   binding GLFW-b 0.2.*
charToSpecial :: Char -> Key
charToSpecial c = case (fromEnum c) of
        32    -> SpecialKey KeySpace
        63232 -> SpecialKey KeyUp
        63233 -> SpecialKey KeyDown
        63234 -> SpecialKey KeyLeft
        63235 -> SpecialKey KeyRight
        63236 -> SpecialKey KeyF1
        63237 -> SpecialKey KeyF2
        63238 -> SpecialKey KeyF3
        63239 -> SpecialKey KeyF4
        63240 -> SpecialKey KeyF5
        63241 -> SpecialKey KeyF6
        63242 -> SpecialKey KeyF7
        63243 -> SpecialKey KeyF8
        63244 -> SpecialKey KeyF9
        63245 -> SpecialKey KeyF10
        63246 -> SpecialKey KeyF11
        63247 -> SpecialKey KeyF12
        63248 -> SpecialKey KeyF13
        63272 -> SpecialKey KeyDelete
        63273 -> SpecialKey KeyHome
        63275 -> SpecialKey KeyEnd
        63276 -> SpecialKey KeyPageUp
        63277 -> SpecialKey KeyPageDown
        _     -> Char c

instance GLFWKey GLFW.MouseButton where
  fromGLFW mouse
   = case mouse of
        GLFW.MouseButton'1 -> MouseButton LeftButton
        GLFW.MouseButton'2 -> MouseButton RightButton
        GLFW.MouseButton'3 -> MouseButton MiddleButton
        GLFW.MouseButton'4 -> MouseButton $ AdditionalButton 3
        GLFW.MouseButton'5 -> MouseButton $ AdditionalButton 4
        GLFW.MouseButton'6 -> MouseButton $ AdditionalButton 5
        GLFW.MouseButton'7 -> MouseButton $ AdditionalButton 6
        GLFW.MouseButton'8 -> MouseButton $ AdditionalButton 7


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
