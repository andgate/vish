module Vish.Renderer.Data.Picture where

import Data.Monoid

data Vector2i = Vector2i Float Float

type Size = Vector2i
type Pos = Vector2i

data Picture =
  Blank
  | Image String
  | Scale Vector2i
  | Translate Vector2i
  | Rotate Vector2i
  | Text
  | Pictures [Picture] -- renders from the first to the last

instance Monoid Picture where
  mempty = Blank
  mappend a (Blank) = a
  mappend (Blank) b = b
  mappend (Pictures a) b = Pictures (a ++ [b])
  mappend a (Pictures b) = Pictures (a:b)
  mappend a b = Pictures [a, b]
  mconcat = Pictures
