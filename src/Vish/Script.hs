{-# LANGUAGE DeriveFunctor #-}

module Vish.Script where

import Data.Maybe
import Data.Functor
import Control.Applicative
import Data.List as List
import qualified Data.Set as S
import qualified Data.HashTable.IO as H
import Control.Monad.Free
import qualified Data.Text as T
import qualified Data.Attoparsec.Text as P

import qualified Data.List.Zipper as Z

type Flags = H.BasicHashTable String Bool
type Background = String
type Name = String
type Expression = String

type Actor = (Name, Expression)

type ScriptCommand = Command ()

type Script = Free Command ()

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

data Scene = Scene Name Script
  deriving Show

showActor :: Name -> Expression -> Script
showActor c e = liftF $ ShowActor (c, e) ()

showActors :: Name -> Expression -> Name -> Expression -> Script
showActors l e1 r e2 =
  liftF $ ShowActors (l, e1) (r, e2) ()

hideActors :: Script
hideActors = liftF $ HideActors ()

pause :: Double -> Script
pause s = liftF $ Pause s ()

setBackground :: Background -> Script
setBackground bg = liftF $ SetBackground bg ()

speak :: String -> String -> Script
speak name msg = liftF $ Speak name msg ()

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
nextCommand (Free (ShowActor c next)) =
  (ShowActor c (), next)

nextCommand (Free (ShowActors l r next)) =
  (ShowActors l r (), next)

nextCommand (Free (HideActors next)) =
  (HideActors (), next)

nextCommand (Free (SetBackground bg next)) =
  (SetBackground bg (), next)

nextCommand (Free (Pause s next)) =
  (Pause s (), next)

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

maybeNextCommand :: Script -> Maybe (Command (), Script)
maybeNextCommand (Pure _) = Nothing
maybeNextCommand (Free Done) = Nothing
maybeNextCommand s = Just $ nextCommand s

scriptToList :: Script -> [ScriptCommand]
scriptToList = List.unfoldr maybeNextCommand

scriptToZipper :: Script -> Z.Zipper ScriptCommand
scriptToZipper = Z.fromList . scriptToList

getActors :: [ScriptCommand] -> [Actor]
getActors = S.toList . foldr S.insert S.empty . extractActors

actorTag :: (Name, Expression) -> String
actorTag (name, expr) = name ++ "_" ++ expr

extractActors :: [ScriptCommand] -> [Actor]
extractActors = concatMap cmdToActors

cmdToActors :: Command () -> [Actor]
cmdToActors (ShowActor c _) =
  [c]
cmdToActors (ShowActors l r _) =
  [l, r]
cmdToActors (Speak name msg _) =
  fmap (\e -> (name,e)) . getActorExprs . parseActorMsg . T.pack $ msg
cmdToActors _ =
  []

getBgs :: [ScriptCommand] -> [Background]
getBgs = S.toList . foldr S.insert S.empty . extractBgs

extractBgs :: [ScriptCommand] -> [Background]
extractBgs = mapMaybe cmdToBg

cmdToBg :: Command () -> Maybe Background
cmdToBg (SetBackground bg _) = Just bg
cmdToBg _ = Nothing

data ActorMessage = ActorMessage String | ActorExpression String

getActorExpr :: ActorMessage -> Maybe Expression
getActorExpr (ActorExpression e) = Just e
getActorExpr _ = Nothing

getActorExprs :: [ActorMessage] -> [Expression]
getActorExprs = mapMaybe getActorExpr

validateActorMsg :: T.Text -> Bool
validateActorMsg =  even . T.length . T.filter (== '*') . T.replace "\\*" ""

instance Show ActorMessage where
  show msg =
    case msg of
      ActorMessage m    -> m
      ActorExpression e -> "*" ++ e ++ "*"

parseActorMsg :: T.Text -> [ActorMessage]
parseActorMsg msg =
  let result = P.parseOnly actorMsgs msg in
  either (const []) id result
  where
    actorMsgs :: P.Parser [ActorMessage]
    actorMsgs =
      P.many' $ exprEscape <|> actorExpr <|> actorMsg

    exprEscape :: P.Parser ActorMessage
    exprEscape = do
      P.char '\\'
      c <- P.char '*'
      return $ ActorMessage [c]

    actorMsg :: P.Parser ActorMessage
    actorMsg = do
      str <- P.takeWhile1 $ \c -> c /= '*' && c /= '\\'
      return . ActorMessage . T.unpack $ str

    actorExpr :: P.Parser ActorMessage
    actorExpr = do
      P.char '*'
      str <- P.takeWhile1 (/= '*')
      P.char '*'
      return . ActorExpression . T.unpack $ str
