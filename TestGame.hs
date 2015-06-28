module Main where

import Vish
import Vish.Backend.Application

gabe :: Actor
gabe = Actor {
  name = "Gabriel"
}

simpleScript :: Script
simpleScript = do
  setBackground "Raleigh's Basement"
  showActor gabe ":)"
  speak gabe "Hello world."
  speak gabe "*sad*I am the only person here..."
  speak gabe "*contemplate*Well, *happy*it works!"
  hideActor gabe
  done

main :: IO ()
main = do
  printScript simpleScript
  play
