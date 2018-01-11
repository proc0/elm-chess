module Data.Game exposing (..)

type alias GameModel =
    { board : Board 
    }

type alias Board 
    = List Rank

type alias Rank 
    = List Square

type Square
    = Vacant
    | Occupied Piece

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

--

type alias History 
    = List Move

type alias Move =
    ( Position
    , Position 
    )

type alias Position =
    { x : Int
    , y : Int 
    }
