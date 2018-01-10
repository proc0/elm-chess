module Data.Main exposing (..)

import Mouse exposing (..)
import Data.Game exposing (..)

fst = Tuple.first
snd = Tuple.second
(=>) = (,)

type alias Model =
    { game : GameModel
    }

type Msg
    = SquareClicked Mouse.Position