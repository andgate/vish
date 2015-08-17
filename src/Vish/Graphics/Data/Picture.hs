module Vish.Graphics.Data.Picture where

import Data.Monoid
import Vish.Math.Data.Vector

type Size = Vector2f
type Pos = Vector2f

data Picture =
  Blank
  | Image String Picture
  | SetPosition Pos Picture
  | Move Pos Picture
  | SetSize Size Picture
  | AddFlag DrawFlag Picture
  | Stretch Size Picture
  | Translate Pos Picture
  | Scale Size Picture
  | Rotate Float Picture
  | Text String Picture
  | Pictures [Picture]
  deriving (Show)

instance Monoid Picture where
  mempty = Blank
  mappend (Pictures a) (Pictures b) = Pictures (a++b)
  mappend (Pictures a) b = Pictures (a++[b])
  mappend a (Pictures b) = Pictures (a:b)
  mappend a b = Pictures [a, b]

data DrawFlag = FitArea Size
  deriving (Show)

blank :: Picture
blank = Blank

image :: String -> Picture
image path = Image path blank

imageAtop :: String -> Picture -> Picture
imageAtop = Image

setPosition :: Vector2f -> Picture -> Picture
setPosition = SetPosition

setPositionXY :: Float -> Float -> Picture -> Picture
setPositionXY x y = SetPosition (Vector2f x y)

move :: Vector2f -> Picture -> Picture
move = Move

moveXY :: Float -> Float -> Picture -> Picture
moveXY x y = Move (Vector2f x y)

setSize :: Vector2f -> Picture -> Picture
setSize = SetSize

setSizeWH :: Float -> Float -> Picture -> Picture
setSizeWH w h = SetSize (Vector2f w h)

fitArea :: Vector2f -> Picture -> Picture
fitArea = AddFlag . FitArea

fitAreaXY :: Float -> Float -> Picture -> Picture
fitAreaXY x y = AddFlag . FitArea $ (Vector2f x y)

stretch :: Vector2f -> Picture -> Picture
stretch = Stretch

stretchWH :: Float -> Float -> Picture -> Picture
stretchWH w h = Stretch (Vector2f w h)

translateXY ::  Float -> Float -> Picture -> Picture
translateXY x y = Translate (Vector2f x y)

translate ::  Vector2f -> Picture -> Picture
translate = Translate

rotate :: Float -> Picture -> Picture
rotate = Rotate

scale :: Vector2f -> Picture -> Picture
scale = Scale

text :: String -> Picture
text t = Text t blank
