module Main exposing (..)

import Html exposing (..)
import Debug exposing (..)
import Mouse exposing (..)

import Data.Main exposing (..)
import Frame.Main as Frame exposing (..)
import Model.Main as Model exposing (..)
import View.Main as View exposing (..)

main : Program Never Model Msg
main = Html.program
    { init = init
    , view = View.render
    , update = Frame.update
    , subscriptions = subscriptions
    }

init : ( Model, Cmd Msg )
init = let initBoard = Model.fromFEN initialBoard
       in Model initBoard Nothing ! []

subscriptions : Model -> Sub Msg
subscriptions model = 
    case model.drag of
        Nothing ->
          Sub.none
        Just _ ->
          Sub.batch 
            [ Mouse.moves PieceDrag
            , Mouse.ups PieceDrop 
            ]