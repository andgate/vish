module Vish.Application.Backend.GLUT
  (GLUTState)
where

import Vish.Application.Backend.Types

import Control.Concurrent
import Control.Lens
import Control.Monad

import Data.IORef

import Graphics.UI.GLUT                    (get,($=))
import qualified Graphics.UI.GLUT          as GLUT
import qualified Graphics.Rendering.OpenGL as GL

import qualified System.Exit as System
import qualified System.Mem  as System


-- | We don't maintain any state information for the GLUT backend,
--   so this data type is empty.
data GLUTState = GLUTState

initGLUTState :: GLUTState
initGLUTState  = GLUTState


instance Backend GLUTState where
  initBackendState  = initGLUTState
  initializeBackend = initGLUT

  -- non-freeglut doesn't like this: (\_ -> GLUT.leaveMainLoop)
  exitBackend _ = System.exitWith System.ExitSuccess

  openWindow = openWindowGLUT
  dumpBackendState = dumpStateGLUT
  installDisplayCallback = installDisplayCallbackGLUT

  installWindowCloseCallback = installWindowCloseCallbackGLUT

  installReshapeCallback = installReshapeCallbackGLUT
  installKeyMouseCallback = installKeyMouseCallbackGLUT
  installMotionCallback = installMotionCallbackGLUT
  installIdleCallback = installIdleCallbackGLUT

  -- Call the GLUT mainloop.
  -- This function will return when something calls GLUT.leaveMainLoop
  runMainLoop _   = GLUT.mainLoop
  postRedisplay _ = GLUT.postRedisplay Nothing

  getWindowDimensions _ = do
    GL.Size sizeX sizeY <- get GLUT.windowSize
    return . Just $ (fromEnum sizeX, fromEnum sizeY)

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

  GLUT.initialDisplayMode $= [ GLUT.RGBMode, GLUT.DoubleBuffered]

  -- See if our requested display mode is possible
  displayMode         <- get GLUT.initialDisplayMode
  displayModePossible <- get GLUT.displayModePossible
  when debug . putStrLn $  "  displayMode       = " ++ show displayMode ++ "\n"
                        ++ "  possible          = " ++ show displayModePossible ++ "\n"


-- Open Window ----------------------------------------------------------------
openWindowGLUT :: IORef GLUTState -> Window -> IO ()
openWindowGLUT _ win = do
  GLUT.initialWindowSize
    $= GL.Size (fromIntegral $ win^.winW) (fromIntegral $ win^.winH)

  GLUT.initialWindowPosition
    $= GL.Position (fromIntegral $ win^.winX) (fromIntegral $ win^.winY)

  GLUT.createWindow $ win^.winName

  GLUT.windowSize
    $= GL.Size (fromIntegral $ win^.winW) (fromIntegral $ win^.winH)

  case win^.winState of
    FullScreen -> do
      GLUT.gameModeCapabilities $=
           [ GLUT.Where' GLUT.GameModeWidth GLUT.IsEqualTo $ win^.winW
           , GLUT.Where' GLUT.GameModeHeight GLUT.IsEqualTo $ win^.winH ]
      void GLUT.enterGameMode
    Windowed -> return ()

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

        putStr  $  "* dumpGlutState\n"
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
                ++ "\n"

-- Display Callback -----------------------------------------------------------
installDisplayCallbackGLUT :: IORef GLUTState -> Callbacks -> IO ()
installDisplayCallbackGLUT ref callbacks =
  GLUT.displayCallback $= callbackDisplay ref callbacks

callbackDisplay :: IORef GLUTState -> Callbacks -> IO ()
callbackDisplay ref callbacks = do
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  GL.color $ GL.Color4 0 0 0 (1 :: GL.GLfloat)
  GL.blend $= GL.Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)

  -- get the display callbacks from the chain
  displayCallback callbacks ref

  -- swap front and back buffers
  GLUT.swapBuffers
  -- run gc to reduce pauses during mainloop (hopefully)
  System.performGC

  -- Don't report errors by default.
  -- The windows OpenGL implementation seems to complain for no reason.
  --  GLUT.reportErrors

-- Close Callback -------------------------------------------------------------
-- | Callback for when the user closes the window.
--   We can do some cleanup here.
installWindowCloseCallbackGLUT :: IORef GLUTState -> Callbacks -> IO ()
installWindowCloseCallbackGLUT ref callbacks =
  GLUT.closeCallback $= (Just $ closeCallback callbacks ref)


-- Reshape Callback -----------------------------------------------------------
installReshapeCallbackGLUT :: IORef GLUTState -> Callbacks -> IO ()
installReshapeCallbackGLUT ref callbacks =
  GLUT.reshapeCallback $= Just (callbackReshape ref callbacks)

callbackReshape :: IORef GLUTState -> Callbacks -> GLUT.Size -> IO ()
callbackReshape ref callbacks (GLUT.Size sizeX sizeY) =
  reshapeCallback callbacks ref (fromEnum sizeX, fromEnum sizeY)


-- KeyMouse Callback ----------------------------------------------------------
installKeyMouseCallbackGLUT :: IORef GLUTState -> Callbacks -> IO ()
installKeyMouseCallbackGLUT ref callbacks =
  GLUT.keyboardMouseCallback $= Just (callbackKeyMouse ref callbacks)

callbackKeyMouse :: IORef GLUTState -> Callbacks
                 -> GLUT.Key -> GLUT.KeyState -> GLUT.Modifiers -> GLUT.Position
                 -> IO ()
callbackKeyMouse ref callbacks
                 key keystate modifiers
                 (GLUT.Position posX posY) =
  keyboardMouseCallback callbacks ref key' keyState' modifiers' pos
  where
    key'       = glutKeyToKey key
    keyState'  = glutKeyStateToKeyState keystate
    modifiers' = glutModifiersToModifiers modifiers
    pos        = (fromEnum posX, fromEnum posY)


-- Motion Callback ------------------------------------------------------------
installMotionCallbackGLUT :: IORef GLUTState -> Callbacks -> IO ()
installMotionCallbackGLUT ref callbacks = do
  GLUT.motionCallback        $= Just (callbackMotion ref callbacks)
  GLUT.passiveMotionCallback $= Just (callbackMotion ref callbacks)

callbackMotion :: IORef GLUTState -> Callbacks -> GLUT.Position -> IO ()
callbackMotion ref callbacks (GLUT.Position posX posY) =
  motionCallback callbacks ref pos
  where pos = (fromEnum posX, fromEnum posY)


-- Idle Callback --------------------------------------------------------------
installIdleCallbackGLUT :: IORef GLUTState -> Callbacks -> IO ()
installIdleCallbackGLUT ref callbacks =
  GLUT.idleCallback $= Just (callbackIdle ref callbacks)

callbackIdle :: IORef GLUTState -> Callbacks -> IO ()
callbackIdle ref callbacks =
  idleCallback callbacks ref

-------------------------------------------------------------------------------
-- | Convert GLUTs key codes to our internal ones.
glutKeyToKey :: GLUT.Key -> Key
glutKeyToKey key
 = case key of
        GLUT.Char '\32'                            -> SpecialKey KeySpace
        GLUT.Char '\13'                            -> SpecialKey KeyEnter
        GLUT.Char '\9'                             -> SpecialKey KeyTab
        GLUT.Char '\ESC'                           -> SpecialKey KeyEsc
        GLUT.Char '\DEL'                           -> SpecialKey KeyDelete
        GLUT.Char c                                -> Char c
        GLUT.SpecialKey GLUT.KeyF1                 -> SpecialKey KeyF1
        GLUT.SpecialKey GLUT.KeyF2                 -> SpecialKey KeyF2
        GLUT.SpecialKey GLUT.KeyF3                 -> SpecialKey KeyF3
        GLUT.SpecialKey GLUT.KeyF4                 -> SpecialKey KeyF4
        GLUT.SpecialKey GLUT.KeyF5                 -> SpecialKey KeyF5
        GLUT.SpecialKey GLUT.KeyF6                 -> SpecialKey KeyF6
        GLUT.SpecialKey GLUT.KeyF7                 -> SpecialKey KeyF7
        GLUT.SpecialKey GLUT.KeyF8                 -> SpecialKey KeyF8
        GLUT.SpecialKey GLUT.KeyF9                 -> SpecialKey KeyF9
        GLUT.SpecialKey GLUT.KeyF10                -> SpecialKey KeyF10
        GLUT.SpecialKey GLUT.KeyF11                -> SpecialKey KeyF11
        GLUT.SpecialKey GLUT.KeyF12                -> SpecialKey KeyF12
        GLUT.SpecialKey GLUT.KeyLeft               -> SpecialKey KeyLeft
        GLUT.SpecialKey GLUT.KeyUp                 -> SpecialKey KeyUp
        GLUT.SpecialKey GLUT.KeyRight              -> SpecialKey KeyRight
        GLUT.SpecialKey GLUT.KeyDown               -> SpecialKey KeyDown
        GLUT.SpecialKey GLUT.KeyPageUp             -> SpecialKey KeyPageUp
        GLUT.SpecialKey GLUT.KeyPageDown           -> SpecialKey KeyPageDown
        GLUT.SpecialKey GLUT.KeyHome               -> SpecialKey KeyHome
        GLUT.SpecialKey GLUT.KeyEnd                -> SpecialKey KeyEnd
        GLUT.SpecialKey GLUT.KeyInsert             -> SpecialKey KeyInsert
        GLUT.SpecialKey GLUT.KeyNumLock            -> SpecialKey KeyNumLock
        GLUT.SpecialKey GLUT.KeyBegin              -> SpecialKey KeyBegin
        GLUT.SpecialKey GLUT.KeyDelete             -> SpecialKey KeyDelete
        GLUT.SpecialKey (GLUT.KeyUnknown _)        -> SpecialKey KeyUnknown
        GLUT.SpecialKey GLUT.KeyShiftL             -> SpecialKey KeyShiftL
        GLUT.SpecialKey GLUT.KeyShiftR             -> SpecialKey KeyShiftR
        GLUT.SpecialKey GLUT.KeyCtrlL              -> SpecialKey KeyCtrlL
        GLUT.SpecialKey GLUT.KeyCtrlR              -> SpecialKey KeyCtrlR
        GLUT.SpecialKey GLUT.KeyAltL               -> SpecialKey KeyAltL
        GLUT.SpecialKey GLUT.KeyAltR               -> SpecialKey KeyAltR
        GLUT.MouseButton GLUT.LeftButton           -> MouseButton LeftButton
        GLUT.MouseButton GLUT.MiddleButton         -> MouseButton MiddleButton
        GLUT.MouseButton GLUT.RightButton          -> MouseButton RightButton
        GLUT.MouseButton GLUT.WheelUp              -> MouseButton WheelUp
        GLUT.MouseButton GLUT.WheelDown            -> MouseButton WheelDown
        GLUT.MouseButton (GLUT.AdditionalButton i) -> MouseButton (AdditionalButton i)

-- | Convert GLUTs key states to our internal ones.
glutKeyStateToKeyState :: GLUT.KeyState -> KeyState
glutKeyStateToKeyState state
 = case state of
        GLUT.Down       -> Down
        GLUT.Up         -> Up


-- | Convert GLUTs key states to our internal ones.
glutModifiersToModifiers :: GLUT.Modifiers -> Modifiers

glutModifiersToModifiers (GLUT.Modifiers a b c)
        = Modifiers     (glutKeyStateToKeyState a)
                        (glutKeyStateToKeyState b)
                        (glutKeyStateToKeyState c)
