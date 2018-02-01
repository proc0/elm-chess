module Main exposing (..)

import Html exposing (..)
import Debug exposing (..)
import Mouse exposing (..)

import Data.Chess exposing (..)
import Data.Model exposing (..)
import Frame.Main as Frame exposing (..)
import Frame.Moves exposing (..)
import View.Main as View exposing (..)

main : Program Never Chess Msg
main = Html.program
    { init = init
    , view = View.render
    , update = Frame.update
    , subscriptions = subscriptions
    }

init : ( Chess, Cmd Msg )
init = let initBoard = fromFEN initialBoard
           initPlayer = Player Nothing Nothing
       in Chess initBoard initPlayer [] ! []

subscriptions : Chess -> Sub Msg
subscriptions {player} = 
            case player.drag of 
                Just sq ->
                    Sub.batch 
                        [ Mouse.moves Drag
                        , Mouse.ups Drop 
                        ]                   
                Nothing -> Sub.none

