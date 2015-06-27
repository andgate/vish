{-# LANGUAGE DeriveFunctor #-}

module Vish where

import qualified Data.HashTable.IO as H
import Control.Monad.Free

type Flags = H.BasicHashTable String Bool
type Expression = String
type Background = String

data Character = Character { name :: String }

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
ifNoFlag f s = liftF $ IfNoFlag f s ()

done :: Script
done = liftF Done


runScript :: Script -> IO ()
runScript (Free (ShowCharacter c e next)) =
  do putStrLn $ "Showing " ++ show c ++ " as " ++ show e
     runScript next

runScript (Free (HideCharacter c next)) =
  do putStrLn $ "Hiding " ++ show c ++ "."
     runScript next

runScript (Free (SetBackground bg next)) =
  do putStrLn $ "Background set to " ++ show bg ++ "."
     runScript next

runScript (Free (Speak c str next)) =
  do putStrLn $ show c ++ ": " ++ show str
     runScript next

runScript (Free (SetScene s next)) =
  do putStrLn $ "Entering scene " ++ show s
     runScript next

runScript (Free (SetFlag f p next)) =
  do putStrLn $ "Setting flag " ++ f ++ " to " ++ show p ++ "."
     runScript next

runScript (Free (IfFlag f s next)) =
  do putStrLn $ "If " ++ show f ++ " then run..."
     runScript s
     runScript next

runScript (Free (IfNoFlag f s next)) =
  do putStrLn $ "If no " ++ show f ++ " then run..."
     runScript s
     runScript next

runScript (Free Done) =
  return ()

runScript (Pure _) =
  return ()
