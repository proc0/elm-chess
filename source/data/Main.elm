module Data.Main exposing (..)

import Mouse exposing (..)
import Data.Game as Game exposing (..)

type alias Chess =
    { board   : Board
    , player  : Player
    , history : History
    }

type alias Player =
    { select : Maybe Square
    , drag   : Maybe Square 
    }

type Msg = 
      Click Mouse.Position
    | Drag Mouse.Position
    | Drop Mouse.Position