module Data.Game exposing (..)

import Matrix exposing (..)
import Array exposing (..)

type alias Board =
    Matrix Square

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

