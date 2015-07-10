{-# LANGUAGE DeriveFunctor #-}

module Vish.Script where

import Data.Maybe
import Data.Functor
import Control.Applicative
import Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.HashTable.IO as H
import Control.Monad.Free
import qualified Data.Text as T
import qualified Data.Attoparsec.Text as P

type Flags = H.BasicHashTable String Bool
type Background = String
type Name = String
type Expression = String
type Expressions = Set Expression

data Actor = Actor Name

data ActorState = ActorState Actor Expression
  deriving Show

actorState :: Name -> Expression -> ActorState
actorState n = ActorState (Actor n)

getActorState :: ActorState -> (Name, Expression)
getActorState (ActorState (Actor n) e) = (n, e)

instance Show Actor where
  show (Actor s) = s

type Script = Free Command ()

data Command next =
  Done
  | ShowActor ActorState next
  | ShowActors ActorState ActorState next
  | HideActors next
  | Pause Float next
  | SetBackground Background next
  | Speak Actor String next
  | SetScene Scene next
  | SetFlag String Bool next
  | IfFlag String Script next
  | IfNoFlag String Script next
  deriving(Functor, Show)

data Scene = Scene Name Script
  deriving Show

showActor :: Name -> Expression -> Script
showActor c e = liftF $ ShowActor (actorState c e) ()

showActors :: Name -> Expression -> Name -> Expression -> Script
showActors l e1 r e2 =
  liftF $ ShowActors (actorState l e1) (actorState r e2) ()

hideActors :: Script
hideActors = liftF $ HideActors ()

pause :: Float -> Script
pause s = liftF $ Pause s ()

setBackground :: Background -> Script
setBackground bg = liftF $ SetBackground bg ()

speak :: String -> String -> Script
speak c str = liftF $ Speak (Actor c) str ()

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

scriptToList :: Script -> [Command ()]
scriptToList = List.unfoldr maybeNextCommand

commandReq :: Command () -> [(Name, Expression)]
commandReq (ShowActor c _) =
  [getActorState c]
commandReq (ShowActors l r _) =
  [getActorState l, getActorState r]
commandReq (Speak (Actor c) msg _) =
  fmap (\e -> (c,e)) . getActorMsgExprs . parseActorMsg . T.pack $ msg
commandReq _ =
  []

data ActorMessage = ActorMessage String | ActorExpression String

getActorMsgExpr :: ActorMessage -> Maybe Expression
getActorMsgExpr (ActorExpression e) = Just e
getActorMsgExpr _ = Nothing

getActorMsgExprs :: [ActorMessage] -> [Expression]
getActorMsgExprs = mapMaybe getActorMsgExpr

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

printScript :: Script -> IO ()
printScript = iterM printCommand

printCommand :: Command (IO ()) -> IO ()
printCommand cmd =
  case cmd of
    ShowActor c next -> do
      putStrLn $ "Showing " ++ show c
      next
    ShowActors l r next -> do
      putStrLn $ "Showing " ++ show l ++ " and " ++ show r
      next
    HideActors next -> do
      putStrLn "Hiding actors."
      next
    Pause s next -> do
      putStrLn $ "Pausing for " ++ show s ++ " seconds."
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
