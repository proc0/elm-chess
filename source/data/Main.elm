module Data.Main exposing (..)

import Mouse exposing (..)
import Data.Game exposing (..)

type alias Model =
    { game : GameModel
    , drag : Maybe Drag
    }

type alias Drag =
    { start : Mouse.Position
    , current : Mouse.Position
    }

type Msg = 
      PieceLift Mouse.Position
    | PieceDrag Mouse.Position
    | PieceDrop Mouse.Position