module Data.Type exposing (..)

import Matrix exposing (..)
import Array exposing (..)
import Mouse exposing (..)
import Material

--=======================--

type alias Game =
    { ui      : UI
    , chess   : Chess
    , players : Players
    }

--           ♔           --
--=======================--

type alias Chess =
    { board   : Board
    , history : History
    }

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
    , ellapsed : Int
    , path : List Location
    }

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

--     Interaction       --
--=======================--

type alias Selection =
    { location : Location
    , piece : Piece
    }

type alias Move =
    { start : Location
    , end : Location
    , piece : Piece
    , capture : Maybe Piece
    , enPassant : Bool
    }

type alias Translation = 
    Location -> Location

type Action =
      Moving Selection
    | Undo Selection
    | End Move
    | Idle

type alias History 
    = List Move

--        Actors         --
--=======================--

type alias Player =
    { color  : Color
    , name   : String
    -- performs actions
    , action : Action
    -- captures pieces
    , pieces : List Piece
    }

type alias Players = 
    (Player, Player)

--      Interface        --
--=======================--

type Event = 
      Click Position
    | Drag Position
    | Drop Position
    | Mdl (Material.Msg Event)

type alias UI =
    { mdl : Material.Model
    , turn : String
    }

