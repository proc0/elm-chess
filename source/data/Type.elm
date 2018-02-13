module Data.Type exposing (..)

import Matrix exposing (..)
import Array exposing (..)
import Mouse exposing (..)
import Material

type alias Game =
    { ui      : UI
    , players : Players
    , board   : Board
    , history : History
    }

--        Pieces         --
--=======================--
type Color = 
      White 
    | Black

type Role =
      Pawn
    | Rook
    | Bishop
    | Knight
    | Queen
    | King
    | Zebra

type alias Piece =
    { position : Position
    , color : Color
    , role : Role
    -- is not first move
    , moved : Bool
    }

--        Board          --
--=======================--

type alias Square = 
    { location : Location
    -- maybe occupied
    , piece : Maybe Piece
    -- is potential move
    , valid : Bool
    -- is selected 
    , active : Bool
    }

type alias Rank =
    List Square

type alias Board =
    Matrix Square

--        Player         --
--=======================--

type alias Player =
    { color  : Color
    -- clicks on square
    , select : Maybe Square
    -- drags piece
    , moving : Maybe Piece
    -- has captured pieces
    , captures : List Piece
    }

type alias Players = 
    (Player, Player)

type alias Move =
    { piece : Piece
    -- todo: possible vector?
    , start : Location
    , end : Location
    , capture : Maybe Piece
    }

type alias History 
    = List Move

--          UI           --
--=======================--

type Msg = 
      Click Position
    | Drag Position
    | Drop Position
    | Mdl (Material.Msg Msg)

type alias UI =
    { mdl : Material.Model
    , turn : String
    }

