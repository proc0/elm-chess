module Chess exposing (..)

import Html exposing (..)
import Mouse exposing (..)
import Debug exposing (..)
import Material.Layout as Layout

import Data.Type exposing (..)
import Model.Game as Game exposing (..)
import State.Event as Event exposing (..)
import View.Main as View exposing (..)

main : Program Never Game Event
main = Html.program
    { init = Game.init ! [Layout.sub0 Mdl]
    , view = View.render
    , update = Event.update
    , subscriptions = Game.subscriptions
    }
