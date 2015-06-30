module Vish.Renderer.Data.Picture where

import Data.Monoid

data Vector2f = Vector2f Float Float
  deriving Show

type Size = Vector2f
type Pos = Vector2f

data Picture =
  Blank
  | Image Size String
  | Scale Vector2f
  | Translate Vector2f
  | Rotate Vector2f
  | Text
  | Pictures [Picture] -- renders from the first to the last
  deriving Show

instance Monoid Picture where
  mempty = Blank
  mappend a (Blank) = a
  mappend (Blank) b = b
  mappend (Pictures a) b = Pictures (a ++ [b])
  mappend a (Pictures b) = Pictures (a:b)
  mappend a b = Pictures [a, b]
  mconcat = Pictures
