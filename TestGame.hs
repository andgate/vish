module Main where

import Vish

gabe :: Character
gabe = Character {
  name = "Gabriel"
}

simpleScript :: Script
simpleScript = do
  setBackground "Raleigh's Basement"
  showCharacter gabe ":)"
  speak gabe "Hello world."
  speak gabe "*sad*I am the only person here..."
  speak gabe "*contemplate*Well, *happy*it works!"
  hideCharacter gabe
  done

main :: IO ()
main = runScript simpleScript
