module Main where

import Vish.Script
import Vish.Game

simpleScript :: Script
simpleScript = do
  setBackground "Raleigh's Basement"
  showActor "gabe" "happy"
  speak "gabe" "Hello world."
  speak "gabe" "*sad*I am the only person here..."
  speak "gabe" "Well, *happy*it works!"
  hideActors
  pause 1.5
  done

main :: IO ()
main = do
  printScript simpleScript
  print . validateActorMsg $ "*contemplate*Well, *happy*\\*happyface\\*it works!"
  print . parseActorMsg $ "*contemplate*Well, *happy*\\*happyface\\*it works!"
  runScript simpleScript
