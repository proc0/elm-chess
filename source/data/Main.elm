module Data.Main exposing (..)

import Mouse exposing (..)
import Data.Game as Game exposing (..)

type alias Model =
    { game : GameModel
    , select : Maybe Square
    , player : Maybe Moving
    }

type alias Moving =
    { start : Mouse.Position
    , current : Mouse.Position
    , piece : Maybe Piece
    }

type Msg = 
      Grab Mouse.Position
    | Drag Mouse.Position
    | Drop Mouse.Position
    | PieceDrag Piece Mouse.Position
    | PieceDrop Piece Mouse.Position