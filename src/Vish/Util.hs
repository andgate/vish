module Vish.Util where

fromBool :: Bool -> a -> Maybe a
fromBool True  a = Just a
fromBool False _ = Nothing
