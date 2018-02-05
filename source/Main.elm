module Main exposing (..)

import Html exposing (..)
import Mouse exposing (..)
import Debug exposing (..)
import Material.Layout as Layout

import Data.Type exposing (..)
import Model.Chess as Chess exposing (..)
import State.Event as Event exposing (..)
import View.Main as View exposing (..)

main : Program Never Chess Msg
main = Html.program
    { init = Chess.init ! [Layout.sub0 Mdl]
    , view = View.render
    , update = Event.update
    , subscriptions = Event.subscriptions
    }
