module Chess exposing (..)

import Html exposing (..)
import Data.Type exposing (..)
import State.Game exposing (..)
import View.Main exposing (..)

main : Program Never Game Event
main = Html.program
    { init          = newGame
    , view          = render
    , update        = update
    , subscriptions = subs
    }
