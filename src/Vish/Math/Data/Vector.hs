module Vish.Math.Data.Vector where

data Vector2f = Vector2f Float Float
  deriving Show

instance Num Vector2f where
  (+) (Vector2f x1 y1) (Vector2f x2 y2) =
    Vector2f (x1+x2) (y1+y2)
  (*) (Vector2f x1 y1) (Vector2f x2 y2) =
    Vector2f (x1*x2) (y1*y2)
  abs (Vector2f x y) =
    Vector2f (abs x) (abs y)
  signum (Vector2f x y) =
    Vector2f (signum x) (signum y)
  fromInteger x =
    let x' = fromInteger x
    in Vector2f x' x'
  negate (Vector2f x y) =
    Vector2f (negate x) (negate y)

instance Fractional Vector2f where
  fromRational x =
    let x' = fromRational x
    in Vector2f x' x'
  recip (Vector2f x y) =
    Vector2f (1/x) (1/y)
  (/) (Vector2f x1 y1) (Vector2f x2 y2) =
    Vector2f (x1 / x2) (y1 / y2)
