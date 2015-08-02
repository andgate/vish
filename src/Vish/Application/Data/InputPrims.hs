{-# LANGUAGE DeriveGeneric #-}
module Vish.Application.Data.InputPrims where

import GHC.Generics (Generic)
import Data.Hashable

-------------------------------------------------------------------------------
-- This is Vish's view of mouse and keyboard events.
-- The actual events provided by the backends are converted to this form
-- by the backend module.

data KeyState = Down | Up | Held
  deriving (Show, Eq, Ord)

data MouseButton =
    Left'Button
  | Middle'Button
  | Right'Button
  | Additional'Button Int
  | Unknown'Button
  deriving (Show, Eq, Ord, Generic)

instance Hashable MouseButton

data Key =
    Key'Unknown
  | Key'GraveAccent
  | Key'0
  | Key'1
  | Key'2
  | Key'3
  | Key'4
  | Key'5
  | Key'6
  | Key'7
  | Key'8
  | Key'9
  | Key'Minus
  | Key'Equal
  | Key'Tilde
  | Key'Exclaim
  | Key'At
  | Key'Hash
  | Key'Dollar
  | Key'Percent
  | Key'Caret
  | Key'Ampersand
  | Key'Asterisk
  | Key'LeftParens
  | Key'RightParens
  | Key'Underscore
  | Key'Plus
  | Key'a
  | Key'b
  | Key'c
  | Key'd
  | Key'e
  | Key'f
  | Key'g
  | Key'h
  | Key'i
  | Key'j
  | Key'k
  | Key'l
  | Key'm
  | Key'n
  | Key'o
  | Key'p
  | Key'q
  | Key'r
  | Key's
  | Key't
  | Key'u
  | Key'v
  | Key'w
  | Key'x
  | Key'y
  | Key'z
  | Key'A
  | Key'B
  | Key'C
  | Key'D
  | Key'E
  | Key'F
  | Key'G
  | Key'H
  | Key'I
  | Key'J
  | Key'K
  | Key'L
  | Key'M
  | Key'N
  | Key'O
  | Key'P
  | Key'Q
  | Key'R
  | Key'S
  | Key'T
  | Key'U
  | Key'V
  | Key'W
  | Key'X
  | Key'Y
  | Key'Z
  | Key'LeftBracket
  | Key'RightBracket
  | Key'Backslash
  | Key'Semicolon
  | Key'Apostrophe
  | Key'Comma
  | Key'Period
  | Key'Slash
  | Key'LeftCurlyBracket
  | Key'RightCurlyBracket
  | Key'Pipe
  | Key'Colon
  | Key'Quote
  | Key'LesserThan
  | Key'GreaterThan
  | Key'Question
  | Key'World1
  | Key'World2
  | Key'Escape
  | Key'Enter
  | Key'Tab
  | Key'Backspace
  | Key'Insert
  | Key'Delete
  | Key'Right
  | Key'Left
  | Key'Down
  | Key'Up
  | Key'PageUp
  | Key'PageDown
  | Key'Home
  | Key'End
  | Key'CapsLock
  | Key'ScrollLock
  | Key'NumLock
  | Key'PrintScreen
  | Key'Pause
  | Key'LeftShift
  | Key'LeftControl
  | Key'LeftSuper
  | Key'LeftAlt
  | Key'Space
  | Key'RightShift
  | Key'RightAlt
  | Key'Menu
  | Key'RightSuper
  | Key'RightControl
  | Key'Pad0
  | Key'Pad1
  | Key'Pad2
  | Key'Pad3
  | Key'Pad4
  | Key'Pad5
  | Key'Pad6
  | Key'Pad7
  | Key'Pad8
  | Key'Pad9
  | Key'PadDecimal
  | Key'PadDivide
  | Key'PadMultiply
  | Key'PadSubtract
  | Key'PadAdd
  | Key'PadEnter
  | Key'PadEqual
  | Key'F1
  | Key'F2
  | Key'F3
  | Key'F4
  | Key'F5
  | Key'F6
  | Key'F7
  | Key'F8
  | Key'F9
  | Key'F10
  | Key'F11
  | Key'F12
  | Key'F13
  | Key'F14
  | Key'F15
  | Key'F16
  | Key'F17
  | Key'F18
  | Key'F19
  | Key'F20
  | Key'F21
  | Key'F22
  | Key'F23
  | Key'F24
  | Key'F25
  deriving (Show, Eq, Ord, Generic)

instance Hashable Key

shiftKey :: Key -> Key
shiftKey key =
  case key of
    Key'GraveAccent -> Key'Tilde
    Key'1 -> Key'Exclaim
    Key'2 -> Key'At
    Key'3 -> Key'Hash
    Key'4 -> Key'Dollar
    Key'5 -> Key'Percent
    Key'6 -> Key'Caret
    Key'7 -> Key'Ampersand
    Key'8 -> Key'Asterisk
    Key'9 -> Key'LeftParens
    Key'0 -> Key'RightParens
    Key'Minus -> Key'Underscore
    Key'Equal -> Key'Plus
    Key'a -> Key'A
    Key'b -> Key'B
    Key'c -> Key'C
    Key'd -> Key'D
    Key'e -> Key'E
    Key'f -> Key'F
    Key'g -> Key'G
    Key'h -> Key'H
    Key'i -> Key'I
    Key'j -> Key'J
    Key'k -> Key'K
    Key'l -> Key'L
    Key'm -> Key'M
    Key'n -> Key'N
    Key'o -> Key'O
    Key'p -> Key'P
    Key'q -> Key'Q
    Key'r -> Key'R
    Key's -> Key'S
    Key't -> Key'T
    Key'u -> Key'U
    Key'v -> Key'V
    Key'w -> Key'W
    Key'x -> Key'X
    Key'y -> Key'Y
    Key'z -> Key'Z
    Key'LeftBracket -> Key'LeftCurlyBracket
    Key'RightBracket -> Key'RightCurlyBracket
    Key'Backslash -> Key'Pipe
    Key'Semicolon -> Key'Colon
    Key'Apostrophe -> Key'Quote
    Key'Comma -> Key'LesserThan
    Key'Period -> Key'GreaterThan
    Key'Slash -> Key'Question
    _ -> key -- cannot be shifted
