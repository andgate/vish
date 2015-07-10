module Main where

import Vish.Script
import Vish.Backend.Application

simpleScript :: Script
simpleScript = do
  setBackground "Raleigh's Basement"
  showActor "gabe" ":)"
  speak "gabe" "Hello world."
  speak "gabe" "*sad*I am the only person here..."
  speak "gabe" "*contemplate*Well, *happy*it works!"
  hideActors
  pause 1.5
  done

main :: IO ()
main = do
  printScript simpleScript
  putStrLn . show . getActorExprs . parseActorMsg $ "*contemplate*Well, *happy*\\*happyface\\*it works!"
  play
