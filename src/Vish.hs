{-# LANGUAGE DeriveFunctor #-}

module Vish where

import qualified Data.HashTable.IO as H
import Control.Monad.Free

type Flags = H.BasicHashTable String Bool
type Expression = String
type Background = String
type Name = String

data Character = Character { name :: Name }

instance Show Character where
  show (Character s) = s

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

data Scene = Scene Name Script
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
ifNoFlag f s = liftF $ IfNoFlag f s ()

done :: Script
done = liftF Done

nextCommand :: Script -> (Command (), Script)
nextCommand (Free (ShowCharacter c e next)) =
  (ShowCharacter c e (), next)

nextCommand (Free (HideCharacter c next)) =
  (HideCharacter c (), next)

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

nextCommand (Free Done) =
  (Done, Pure ())
nextCommand (Pure _) =
  (Done, Pure ())

printScript :: Script -> IO ()
printScript (Free (ShowCharacter c e next)) =
  do putStrLn $ "Showing " ++ show c ++ " as " ++ show e
     printScript next

printScript (Free (HideCharacter c next)) =
  do putStrLn $ "Hiding " ++ show c ++ "."
     printScript next

printScript (Free (SetBackground bg next)) =
  do putStrLn $ "Background set to " ++ show bg ++ "."
     printScript next

printScript (Free (Speak c str next)) =
  do putStrLn $ show c ++ ": " ++ show str
     printScript next

printScript (Free (SetScene (Scene n s) _)) =
  do putStrLn $ "Entering scene " ++ show n ++ "..."
     printScript s

printScript (Free (SetFlag f p next)) =
  do putStrLn $ "Setting flag " ++ f ++ " to " ++ show p ++ "."
     printScript next

printScript (Free (IfFlag f s next)) =
  do putStrLn $ "If " ++ show f ++ " then do..."
     printScript s
     printScript next

printScript (Free (IfNoFlag f s next)) =
  do putStrLn $ "If no " ++ show f ++ " then run..."
     printScript s
     printScript next

printScript (Free Done) =
  return ()

printScript (Pure _) =
  return ()
