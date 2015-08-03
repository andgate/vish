module Main where

import Vish.Script
import Vish.Game

simpleScript :: Script
simpleScript = do
  setBackground "Jotaro's Room"
  showActor "Jotaro" "cool"
  pause 1.5
  speak "Jotaro" "Yare yare daze"
  done

main :: IO ()
main = do
  runScript simpleScript
