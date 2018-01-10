module Data.Game exposing (..)

type alias Board = List Rank

type alias Rank = List Square

type Square
    = Vacant
    | Occupied Color Piece

type Color
    = White
    | Black

type Piece
    = Pawn
    | Rook
    | Bishop
    | Knight
    | Queen
    | King

type alias Position =
    { x : Int
    , y : Int 
    }

type alias Move =
    ( Position
    , Position 
    )

type alias History =
    List Move

type alias GameModel =
    { board : Board
    }
