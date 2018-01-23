module Data.Game exposing (..)

type alias Chess =
    { board : Board
    , history : History
    }

type alias Board 
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

