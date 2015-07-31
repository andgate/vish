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

        -- | Latest mouse position
        , _mousePosition :: (Double,Double)

        -- | Latest mousewheel position
        , _mouseWheelPos :: (Double, Double)

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
    , _mousePosition = (0, 0)
    , _mouseWheelPos = (0, 0)
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
  installDisplayCallback     = installDisplayCallbackGLFW
  installWindowCloseCallback = installWindowCloseCallbackGLFW
  installReshapeCallback     = installReshapeCallbackGLFW
  installKeyMouseCallback    = installKeyMouseCallbackGLFW
  installMotionCallback      = installMotionCallbackGLFW
  installIdleCallback        = installIdleCallbackGLFW
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

-- KeyMouse -----------------------------------------------------------------------
-- | Callbacks for when the user presses a key or moves / clicks the mouse.
--   This is a bit verbose because we have to do impedence matching between
--   GLFW's event system, and the one use by Gloss which was originally
--   based on GLUT. The main problem is that GLUT only provides a single callback
--   slot for character keys, arrow keys, mouse buttons and mouse wheel movement,
--   while GLFW provides a single slot for each.
--
installKeyMouseCallbackGLFW :: IORef GLFWState -> Callbacks -> IO ()
installKeyMouseCallbackGLFW ref callbacks =
  whenWindow ref $ \glfwWin -> do
    GLFW.setKeyCallback glfwWin         $ Just (callbackKeyboard ref callbacks)
    GLFW.setCharCallback glfwWin        $ Just (callbackChar ref callbacks)
    GLFW.setMouseButtonCallback glfwWin $ Just (callbackMouseButton ref callbacks)
    GLFW.setScrollCallback glfwWin      $ Just (callbackMouseWheel ref callbacks)


-- GLFW calls this on a non-character keyboard action.
callbackKeyboard :: IORef GLFWState -> Callbacks
                  -> GLFW.Window -> GLFW.Key -> Int
                  -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
callbackKeyboard ref callbacks _ key _ keystate _ = do
  let key'      = fromGLFW key
      keystate' = fromGLFW keystate

  setModifiers ref key keystate'
  (mods, pos) <- readIORef ref >>= \s -> return (s^.modifiers, s^.mousePosition)
  keyboardMouseCallback callbacks ref key' keystate' mods pos

setModifiers :: IORef GLFWState -> GLFW.Key -> KeyState -> IO ()
setModifiers ref key keystate =
  modifyIORef ref $ modifiers %~
    case key of
      GLFW.Key'LeftShift    -> shift .~ keystate
      GLFW.Key'LeftControl  -> ctrl .~ keystate
      GLFW.Key'LeftAlt      -> alt .~ keystate
      _                     -> id


-- GLFW calls this on a when the user presses or releases a character key.
callbackChar :: IORef GLFWState -> Callbacks -> GLFW.Window -> Char -> IO ()
callbackChar ref callbacks _ c = do
  let key'      = charToSpecial c
  (mods, pos) <- readIORef ref >>= \s -> return (s^.modifiers, s^.mousePosition)
  keyboardMouseCallback callbacks ref key' Down mods pos


-- GLFW calls on this when the user clicks or releases a mouse button.
callbackMouseButton :: IORef GLFWState -> Callbacks -> GLFW.Window
                    -> GLFW.MouseButton -> GLFW.MouseButtonState
                    -> GLFW.ModifierKeys -> IO ()
callbackMouseButton ref callbacks _ key keystate mods = do
  let key'      = fromGLFW key
      keystate' = fromGLFW keystate
      mods'     = fromGLFW mods

  pos <- liftM (^.mousePosition) (readIORef ref)
  keyboardMouseCallback callbacks ref key' keystate' mods' pos


-- GLFW calls on this when the user moves the mouse wheel.
callbackMouseWheel :: IORef GLFWState -> Callbacks -> GLFW.Window
                   -> Double -> Double -> IO ()
callbackMouseWheel ref callbacks _ x y = do
  (key, keystate)  <- setMouseWheel ref x y
  (mods, pos) <- readIORef ref >>= \s -> return (s^.modifiers, s^.mousePosition)
  keyboardMouseCallback callbacks ref key keystate mods pos

setMouseWheel :: IORef GLFWState -> Double -> Double -> IO (Key, KeyState)
setMouseWheel ref x y = do
  glfwState <- readIORef ref
  modifyIORef ref $ mouseWheelPos .~ (x, y)
  -- compare the old state against the new state
  case compare (x,y) (_mouseWheelPos glfwState) of
          LT -> return (MouseButton WheelDown , Down)
          GT -> return (MouseButton WheelUp   , Down)
          EQ -> return (SpecialKey  KeyUnknown, Up  )


-- Motion Callback ------------------------------------------------------------
-- | Callback for when the user moves the mouse.
installMotionCallbackGLFW :: IORef GLFWState -> Callbacks -> IO ()
installMotionCallbackGLFW ref callbacks =
  whenWindow ref $ \glfwWin ->
    GLFW.setCursorPosCallback glfwWin $ Just (callbackMotion ref callbacks)

callbackMotion :: IORef GLFWState -> Callbacks -> GLFW.Window
                -> Double -> Double -> IO ()
callbackMotion ref callbacks _ x y = do
  modifyIORef ref $ mousePosition .~ (x,y)
  motionCallback callbacks ref (x,y)


-- Idle Callback --------------------------------------------------------------
-- | Callback for when GLFW has finished its jobs and it's time for us to do
--   something for our application.
installIdleCallbackGLFW :: IORef GLFWState -> Callbacks -> IO ()
installIdleCallbackGLFW ref callbacks =
  modifyIORef ref $ idle .~ idleCallback callbacks ref


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
  fromGLFW key
   = case key of
        --GLFW.CharKey c      -> charToSpecial (toLower c)
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
        GLFW.Key'Delete      -> SpecialKey KeyDelete
        GLFW.Key'PageUp      -> SpecialKey KeyPageUp
        GLFW.Key'PageDown    -> SpecialKey KeyPageDown
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
charToSpecial c =
  case fromEnum c of
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

instance GLFWConv GLFW.MouseButton Key where
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
