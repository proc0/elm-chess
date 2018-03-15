module Chess exposing (main)

import Html exposing (program)
import Data.Type exposing (Game, Event)
import State.Game exposing (newGame, update, subscribe)
import View.Main exposing (render)

--             ♘             --

main : Program Never Game Event
main = Html.program
    { init          = newGame
    , view          = render
    , update        = update
    , subscriptions = subscribe
    }
    