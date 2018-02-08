module Data.Type exposing (..)

import Matrix exposing (..)
import Array exposing (..)
import Mouse exposing (..)
import Material

type alias Game =
    { ui : UI
    , players : Players
    , board   : Board
    , turn    : Turn
    , history : History
    }

type Msg = 
      Click Mouse.Position
    | Drag Mouse.Position
    | Drop Mouse.Position
    | Mdl (Material.Msg Msg)

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

type alias Point =
    { x : Int
    , y : Int 
    }

type alias Piece =
    { color  : Color
    , role   : Role
    , point  : Point
    , moved  : Bool
    }

type alias Square = 
    { point  : Point
    , piece  : Maybe Piece
    , valid  : Bool
    , active : Bool
    }

type alias Move =
    { piece : Piece
    , start : Point
    , end : Point
    }

type Moving = 
      Touch Piece
    | Lift Piece
    | End Move
    | Pass

type alias Turn =
    { player : Player
    , select : Maybe Square
    , moving : Maybe Moving
    }

type alias History 
    = List Move

type alias Player =
    { color  : Color
    , pieces : List Piece
    }

type alias Players = 
        (Player, Player)

type alias Rank =
        List Square

type alias Board =
        Matrix Square

type alias UI =
    { mdl  : Material.Model
    , turn : String
    }

