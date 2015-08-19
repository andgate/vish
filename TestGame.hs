module Main where

import Vish.Script
import qualified Vish as Vish

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
  Vish.play simpleScript
