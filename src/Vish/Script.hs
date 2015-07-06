{-# LANGUAGE DeriveFunctor #-}

module Vish.Script where

import qualified Data.HashTable.IO as H
import Control.Monad.Free

type Flags = H.BasicHashTable String Bool
type Expression = String
type Background = String
type Name = String

data Actor = Actor { name :: Name }

instance Show Actor where
  show (Actor s) = s

type Script = Free Command ()

data Command next =
  Done
  | ShowActor Actor Expression next
  | HideActor Actor next
  | SetBackground Background next
  | Speak Actor String next
  | SetScene Scene next
  | SetFlag String Bool next
  | IfFlag String Script next
  | IfNoFlag String Script next
  deriving(Functor, Show)

data Scene = Scene Name Script
  deriving Show

showActor :: Actor -> Expression -> Script
showActor c e = liftF $ ShowActor c e ()

hideActor :: Actor -> Script
hideActor c = liftF $ HideActor c ()

setBackground :: Background -> Script
setBackground bg = liftF $ SetBackground bg ()

speak :: Actor -> String -> Script
speak c str = liftF $ Speak c str ()

setScene :: Scene -> Script
setScene s = liftF $ SetScene s ()

setFlag :: String -> Bool -> Script
setFlag f p = liftF $ SetFlag f p ()

ifFlag :: String -> Script -> Script
ifFlag f s = liftF $ IfFlag f s ()

ifNoFlag :: String -> Script -> Script
ifNoFlag f s = liftF $ IfNoFlag f s ()

done :: Script
done = liftF Done

nextCommand :: Script -> (Command (), Script)
nextCommand (Free (ShowActor c e next)) =
  (ShowActor c e (), next)

nextCommand (Free (HideActor c next)) =
  (HideActor c (), next)

nextCommand (Free (SetBackground bg next)) =
  (SetBackground bg (), next)

nextCommand (Free (Speak c str next)) =
  (Speak c str (), next)

nextCommand (Free (SetScene s next)) =
  (SetScene s (), next)

nextCommand (Free (SetFlag f p next)) =
  (SetFlag f p (), next)

nextCommand (Free (IfFlag f s next)) =
  (IfFlag f s (), next)

nextCommand (Free (IfNoFlag f s next)) =
  (IfNoFlag f s (), next)

nextCommand (Pure _) =
  (Done, Pure ())
nextCommand (Free Done) =
  (Done, Pure ())



printScript :: Script -> IO ()
printScript = iterM printCommand

printCommand :: Command (IO ()) -> IO ()
printCommand cmd =
  case cmd of
    ShowActor c e next -> do
      putStrLn $ "Showing " ++ show c ++ " as " ++ show e
      next
    HideActor c next -> do
      putStrLn $ "Hiding " ++ show c ++ "."
      next
    SetBackground bg next -> do
      putStrLn $ "Background set to " ++ show bg ++ "."
      next
    Speak c str next -> do
      putStrLn $ show c ++ ": " ++ show str
      next
    SetScene (Scene n s) next -> do
      putStrLn $ "Entering scene " ++ show n ++ "..."
      printScript s
      next
    SetFlag f p next -> do
      putStrLn $ "Setting flag " ++ f ++ " to " ++ show p ++ "."
      next
    IfFlag f s next -> do
      putStrLn $ "If " ++ show f ++ " then do..."
      printScript s
      next
    IfNoFlag f s next -> do
      putStrLn $ "If no " ++ show f ++ " then run..."
      printScript s
      next
    Done -> return ()
