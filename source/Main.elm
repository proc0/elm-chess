module Main exposing (..)

import Html exposing (..)
import Debug exposing (..)
import Mouse exposing (..)

import Data.Main exposing (..)
import Data.Game exposing (..)
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
init = let initBoard = fromFEN initialBoard
       in Model initBoard Nothing Nothing ! []

subscriptions : Model -> Sub Msg
subscriptions {player} = 
    case player of
        Nothing -> Sub.none
        Just {piece} ->
            case piece of 
                Just p ->
                    Sub.batch 
                        [ Mouse.moves (Drag p)
                        , Mouse.ups (Drop p) 
                        ]                    
                Nothing -> Sub.none
