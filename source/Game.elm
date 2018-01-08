module Game exposing (..)

import Array exposing (Array)
import Char exposing (..)

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

type Square
    = Vacant
    | Occupied Color Piece

type alias Rank = Array Square

type alias Board = Array Rank

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

type alias Model =
    { board : Board
    --, whitesMove : Bool
    --, whiteQueenCastle : Bool
    --, whiteKingCastle : Bool
    --, blackQueenCastle : Bool
    --, blackKingCastle : Bool
    --, enPassant : String
    --, halfMove : Int
    --, fullMove : Int
    }
