module Main exposing (..)

import Html exposing (..)
import Debug exposing (..)
import Mouse exposing (..)

import Data.Main exposing (..)
import Data.Game exposing (..)
import Frame.Main as Frame exposing (..)
import View.Main as View exposing (..)
import Notation.FEN as FEN exposing (..)

main : Program Never Model Msg
main = Html.program
    { init = init
    , view = View.render
    , update = Frame.update
    , subscriptions = subscriptions
    }

init : ( Model, Cmd Msg )
init = let initBoard = FEN.toModel initialBoard
           initPosition = Mouse.Position 200 200
       in Model initBoard initPosition Nothing ! []

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