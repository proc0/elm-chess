module Data.Chess exposing (..)

import Matrix exposing (..)
import Array exposing (..)
import Mouse exposing (..)

type Msg = 
      Click Mouse.Position
    | Drag Mouse.Position
    | Drop Mouse.Position

type alias Chess =
    { board   : Board
    , player  : Player
    , history : History
    }

type alias Board =
    Matrix Square

type alias Player =
    { select : Maybe Square
    , drag   : Maybe Square 
    }

type alias Rank 
    = List Square

type alias History 
    = List Move

type alias Move =
    (Square, Maybe Square)

type alias Square = 
    { position : Position
    , piece : Maybe Piece
    , valid : Bool
    }

type alias Position =
    { x : Int
    , y : Int 
    }

type alias Piece =
    { color  : Color
    , role   : Role
    , active : Bool
    }

type Color = 
    White | Black

type Role
    = Pawn
    | Rook
    | Bishop
    | Knight
    | Queen
    | King
    | Zebra

