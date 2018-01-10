module Main exposing (..)

import Html exposing (..)
import Debug exposing (..)

import Data.Main exposing (..)
import Data.Game exposing (..)
import Time.Main as Time exposing (..)
import View.Main as View exposing (..)
import Notation.FEN as FEN exposing (..)

main : Program Never Model Msg
main = Html.program
    { init = init
    , view = View.board
    , update = Time.update
    , subscriptions = subscriptions
    }

init : ( Model, Cmd Msg )
init = Model (FEN.toModel initialBoard) ! []

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none
