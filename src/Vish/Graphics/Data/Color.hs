module Vish.Graphics.Data.Color where

import Data.Word

data Color =
  Color
  { r :: !Word8
  , g :: !Word8
  , b :: !Word8
  , a :: !Word8
  }


black :: Color
black = Color 0 0 0 255

white :: Color
white = Color 255 255 255 255

red :: Color
red = Color 255 0 0 255

green :: Color
green = Color 0 255 0 255

blue :: Color
blue = Color 0 0 255 255
