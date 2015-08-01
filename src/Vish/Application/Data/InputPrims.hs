{-# LANGUAGE DeriveGeneric #-}
module Vish.Application.Data.InputPrims where

import Control.Lens
import GHC.Generics (Generic)
import Data.Hashable

-------------------------------------------------------------------------------
-- This is Vish's view of mouse and keyboard events.
-- The actual events provided by the backends are converted to this form
-- by the backend module.

data KeyState = Down | Up | Held
  deriving (Show, Eq, Ord)

data MouseButton =
    LeftButton
  | MiddleButton
  | RightButton
  | AdditionalButton Int
  deriving (Show, Eq, Ord, Generic)

instance Hashable MouseButton

data Key =
    KeyUnknown
  | KeySpace
  | KeyApostrophe
  | KeyComma
  | KeyMinus
  | KeyPeriod
  | KeySlash
  | Key0
  | Key1
  | Key2
  | Key3
  | Key4
  | Key5
  | Key6
  | Key7
  | Key8
  | Key9
  | KeySemicolon
  | KeyEqual
  | KeyA
  | KeyB
  | KeyC
  | KeyD
  | KeyE
  | KeyF
  | KeyG
  | KeyH
  | KeyI
  | KeyJ
  | KeyK
  | KeyL
  | KeyM
  | KeyN
  | KeyO
  | KeyP
  | KeyQ
  | KeyR
  | KeyS
  | KeyT
  | KeyU
  | KeyV
  | KeyW
  | KeyX
  | KeyY
  | KeyZ
  | KeyLeftBracket
  | KeyBackslash
  | KeyRightBracket
  | KeyGraveAccent
  | KeyWorld1
  | KeyWorld2
  | KeyEscape
  | KeyEnter
  | KeyTab
  | KeyBackspace
  | KeyInsert
  | KeyDelete
  | KeyRight
  | KeyLeft
  | KeyDown
  | KeyUp
  | KeyPageUp
  | KeyPageDown
  | KeyHome
  | KeyEnd
  | KeyCapsLock
  | KeyScrollLock
  | KeyNumLock
  | KeyPrintScreen
  | KeyPause
  | KeyF1
  | KeyF2
  | KeyF3
  | KeyF4
  | KeyF5
  | KeyF6
  | KeyF7
  | KeyF8
  | KeyF9
  | KeyF10
  | KeyF11
  | KeyF12
  | KeyF13
  | KeyF14
  | KeyF15
  | KeyF16
  | KeyF17
  | KeyF18
  | KeyF19
  | KeyF20
  | KeyF21
  | KeyF22
  | KeyF23
  | KeyF24
  | KeyF25
  | KeyPad0
  | KeyPad1
  | KeyPad2
  | KeyPad3
  | KeyPad4
  | KeyPad5
  | KeyPad6
  | KeyPad7
  | KeyPad8
  | KeyPad9
  | KeyPadDecimal
  | KeyPadDivide
  | KeyPadMultiply
  | KeyPadSubtract
  | KeyPadAdd
  | KeyPadEnter
  | KeyPadEqual
  | KeyLeftShift
  | KeyLeftControl
  | KeyLeftAlt
  | KeyLeftSuper
  | KeyRightShift
  | KeyRightControl
  | KeyRightAlt
  | KeyRightSuper
  | KeyMenu
  deriving (Show, Eq, Ord, Generic)

instance Hashable Key

data Modifiers =
  Modifiers
  { _shift :: KeyState
  , _ctrl  :: KeyState
  , _alt   :: KeyState
  }
  deriving (Show, Eq, Ord)

makeLenses ''Modifiers
