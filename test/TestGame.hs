module Main where

import Vish.Script
import qualified Vish as Vish

jotaro :: String
jotaro = "Jotaro"

jotarosRoom :: String
jotarosRoom = "Jotaro's Room"

cool :: String
cool = "cool"

simpleScript :: Script
simpleScript = do
  setBackground jotarosRoom
  showActor jotaro cool
  --pause 1.5
  speak jotaro "Yare yare daze"

  showActors jotaro cool jotaro cool
  speak jotaro "Snooty snooty daze"

  showActor jotaro cool
  speak jotaro "Raleigh give me words"
  speak jotaro "I'm begging you!"
  done

main :: IO ()
main =
  Vish.play simpleScript
