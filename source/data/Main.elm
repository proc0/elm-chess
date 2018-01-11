module Data.Main exposing (..)

import Mouse exposing (..)
import Data.Game exposing (..)

type alias Model =
    { game : GameModel
    }

type Msg = 
    SquareClicked Mouse.Position