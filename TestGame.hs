module Main where

import Vish.Script
import Vish.Game

simpleScript :: Script
simpleScript = do
  setBackground "Jotaro's Room"
  showActor "Jotaro" "cool"
  speak "Jotaro" "Yare yare daze"
  pause 1.5
  done

main :: IO ()
main = do
  runScript simpleScript
