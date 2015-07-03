module Vish.Graphics.Data.Picture where

import Data.Monoid

data Vector2f = Vector2f Float Float
  deriving Show

type Size = Vector2f
type Pos = Vector2f

data Picture =
  Blank
  | Image String Picture
  | Scale Vector2f Picture
  | Translate Vector2f Picture
  | Rotate Vector2f Picture
  | Text String Picture
  | Pictures [Picture]
  deriving (Show)

instance Monoid Picture where
  mempty = Blank
  mappend (Pictures a) (Pictures b) = Pictures (a++b)
  mappend (Pictures a) b = Pictures (a++[b])
  mappend a (Pictures b) = Pictures (a:b)
  mappend a b = Pictures [a, b]

blank :: Picture
blank = Blank

image :: String -> Picture
image path = Image path blank

imageAtop :: Picture -> String -> Picture
imageAtop a path = Image path a

translateXY ::  Float -> Float -> Picture -> Picture
translateXY x y = Translate (Vector2f x y)

translate ::  Vector2f -> Picture -> Picture
translate = Translate

rotate :: Picture -> Vector2f -> Picture
rotate pic v = Rotate v pic

scale :: Picture -> Vector2f -> Picture
scale pic v = Scale v pic

text :: String -> Picture
text t = Text t blank
