module Data.Main exposing (..)

import Mouse exposing (..)
import Data.Game as Game exposing (..)

type alias Model =
    { game   : Chess
    --, select : Maybe Square
    , player : Maybe Moving
    }

type alias Moving =
    { select : Maybe Square
    , drag : Maybe Square
    }

--type alias Moving =
--    { piece   : Maybe Piece
--    , start   : Mouse.Position
--    , current : Mouse.Position
--    }

type Msg = 
      Click Mouse.Position
    | Drag Square Mouse.Position
    | Drop Square Mouse.Position