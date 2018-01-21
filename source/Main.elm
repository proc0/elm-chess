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
       in Model initBoard (Occupied {x=0,y=0} (Black Rook)) Nothing ! []

subscriptions : Model -> Sub Msg
subscriptions model = 
    case model.player of
        Nothing -> Sub.none
        Just {current,piece} ->
            case piece of 
                 Just pc ->
                    Sub.batch 
                        [ Mouse.moves (PieceDrag pc)
                        , Mouse.ups (PieceDrop pc) 
                        ]                    
                 Nothing ->
                    Sub.batch 
                        [ Mouse.moves Drag
                        , Mouse.ups Drop 
                        ]
