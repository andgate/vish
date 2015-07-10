module Vish.UI.Scene where

data Scene = Scene [Widget]

data Widget =
  Empty
  | HTable [Widget]
  | VTable [Widget]
  | Table [[Widget]]
  | Image
  | Button
  | TextBox
