module Data.Type exposing (..)

import Matrix exposing (..)
import Array exposing (..)
import Mouse exposing (..)
import Material

type Msg = 
      Click Mouse.Position
    | Drag Mouse.Position
    | Drop Mouse.Position
    | Mdl (Material.Msg Msg)

type alias Chess =
    { board   : Board
    , player  : Player
    , history : History
    , ui      : UI
    }

type alias Player =
    { select : Maybe Square
    , drag   : Maybe Square 
    }

type alias Board =
    Matrix Square

type alias Rank 
    = List Square

type alias Square = 
    { point : Point
    , piece : Maybe Piece
    , valid : Bool
    }

type alias Piece =
    { color  : Color
    , role   : Role
    , active : Bool
    , moved  : Bool
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

type alias Move =
    (Square, Maybe Square)

type alias History 
    = List Move

type alias Point =
    { x : Int
    , y : Int 
    }

type alias UI =
    { mdl : Material.Model
    , turn : String
    }