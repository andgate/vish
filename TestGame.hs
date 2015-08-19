module Main where

import Vish.Script
import Vish.Game

simpleScript :: Script
simpleScript = do
  setFont "arial-black"
  setBackground "Jotaro's Room"
  showActor "Jotaro" "cool"
  showActors "Jotaro" "cool" "Jotaro" "cool"
  --pause 1.5
  speak "Jotaro" "Yare yare daze"
  done

main :: IO ()
main =
  runScript simpleScript
