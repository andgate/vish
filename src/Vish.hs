{-# LANGUAGE DeriveFunctor #-}

module Vish where

import qualified Data.HashTable.IO as H
import Control.Monad.Free

type Flags = H.BasicHashTable String Bool
type Expression = String
type Background = String

data Character = Character { name :: String }
  deriving Show

type Script = Free Command ()

data Command next =
  ShowCharacter Character Expression next
  | HideCharacter Character next
  | SetBackground Background next
  | Speak Character String next
  | SetScene Scene next
  | SetFlag String Bool next
  | IfFlag String Script next
  | IfNoFlag String Script next
  | Done
  deriving(Functor, Show)

data Scene = Scene Flags [Character] Script
  deriving Show

showCharacter :: Character -> Expression -> Script
showCharacter c e = liftF $ ShowCharacter c e ()

hideCharacter :: Character -> Script
hideCharacter c = liftF $ HideCharacter c ()

setBackground :: Background -> Script
setBackground bg = liftF $ SetBackground bg ()

speak :: Character -> String -> Script
speak c str = liftF $ Speak c str ()

setScene :: Scene -> Script
setScene s = liftF $ SetScene s ()

setFlag :: String -> Bool -> Script
setFlag f p = liftF $ SetFlag f p ()

ifFlag :: String -> Script -> Script
ifFlag f s = liftF $ IfFlag f s ()

ifNoFlag :: String -> Script -> Script
ifNoFlag f s = liftF $ IfFlag f s ()

done :: Script
done = liftF Done
