{-# LANGUAGE DeriveFunctor #-}
module Vish.Data.Script where

import Control.Monad.Free

type Background = String
type Name = String
type Expression = String
type Actor = (Name, Expression)

data Command next =
  Done
  | ShowActor Actor next
  | ShowActors Actor Actor next
  | HideActors next
  | Pause Double next
  | SetBackground Background next
  | Speak Name String next
  | SetScene Scene next
  | SetFlag String Bool next
  | IfFlag String Script next
  | IfNoFlag String Script next
  deriving(Functor, Show)

type Script = Free Command ()
type ScriptCommand = Command ()

data Scene = Scene Name Script
  deriving Show
