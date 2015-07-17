module Vish.Util where

(#) :: a -> (a -> b) -> b
(#) = flip ($)

infixl 8 #

fromBool :: Bool -> a -> Maybe a
fromBool True  a = Just a
fromBool False _ = Nothing
