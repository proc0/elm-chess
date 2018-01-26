module Data.Game exposing (..)

import Matrix exposing (..)
import Array exposing (..)

type alias Chess =
    { board : Board
    , history : History
    }

type alias Board =
    Matrix Square

type alias Boarde 
    = List Rank

type alias Rank 
    = List Square

type alias Square = 
    { position : Position
    , piece : Maybe Piece
    , valid : Bool
    }

type alias Position =
    { x : Int
    , y : Int 
    }

type Piece
    = White Figure
    | Black Figure

type Figure
    = Pawn
    | Rook
    | Bishop
    | Knight
    | Queen
    | King
    | Zebra

--

type alias History 
    = List Move

type alias Move =
    ( Square
    , Square 
    )

