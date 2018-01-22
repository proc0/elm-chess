module Data.Game exposing (..)

type alias GameModel =
    { board : Board
    , history : History
    }

type alias Board 
    = List Rank

type alias Rank 
    = List Square

type alias Square = 
    { pos : Position
    , piece : Maybe Piece
    , hilite : Bool
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

