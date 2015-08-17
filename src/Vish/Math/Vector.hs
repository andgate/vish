module Vish.Math.Vector
  ( module Vish.Math.Vector
  , module Vish.Math.Data.Vector
  )
where

import Vish.Math.Data.Vector

identity :: Vector2f
identity = Vector2f 1 1

zero :: Vector2f
zero = Vector2f 0 0

toTuple :: Vector2f -> (Float, Float)
toTuple (Vector2f x y) = (x,y)

fromTuple :: (Float, Float) -> Vector2f
fromTuple (x,y) = Vector2f x y
