module Main exposing (..)

import Html exposing (..)
import Mouse exposing (..)
import Debug exposing (..)

import Data.Type exposing (..)
import Model.Chess as Chess exposing (..)
import State.Event as Event exposing (..)
import View.Main as View exposing (..)

main : Program Never Chess Msg
main = Html.program
    { init = Chess.init ! []
    , view = View.render
    , update = Event.update
    , subscriptions = Event.subscriptions
    }
