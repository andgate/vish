module Vish.Util where

(#) :: a -> (a -> b) -> b
(#) = flip ($)

infixl 8 #
