module Chess exposing (main)

import Html       exposing (program)
import Data.Type  exposing (Game, Event)
import Data.Pure  exposing (newGame)
import State.Game exposing (update, subscribe)
import View.Main  exposing (render)

--  ♘ 

main : Program Never Game Event
main = program
     { init          = newGame
     , view          = render
     , update        = update
     , subscriptions = subscribe
     }
    