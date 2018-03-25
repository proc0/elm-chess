module Data.Type exposing (..)

import Mouse exposing (Position)
import Keyboard exposing (KeyCode)
import Matrix exposing (Matrix, Location)
import Material exposing (Model, Msg)

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
    | Joker

type alias Piece =
    { point : Location
    , drag : Position
    , color : Color
    , role : Role
    , tick : Int
    , lock : Bool
    , check : Bool
    , path : List Location
    }

type alias Square = 
    { point : Location
    , piece : Maybe Piece
    , valid : Bool
    , active : Bool
    }

type alias Rank =
    List Square

type alias Board =
    Matrix Square

type alias Rule =
    Square -> Bool

--     Interaction       --
--=======================--

type alias Selection =
    { focus : Location
    , piece : Piece
    }

type alias Move =
    { start : Location
    , end : Location
    , piece : Piece
    , capture : Maybe Piece
    , pin : Maybe Piece
    , check : Bool
    , enPassant : Bool
    }

type alias Moves =
    List Move

type alias History = 
    Moves

type alias Translation = 
    Location -> Location

type alias Translations =
    List Translation

type alias Movement =
    Int -> Translation

type alias Movements =
    List (Int -> Translation)
      
type Action =
      Moving Selection
    | Playing Selection
    | End Move
    | Idle

--        Agents         --
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

--       Rules           --
--=======================--

type alias Condition =
    (Translation, Square -> Bool)

--      Interface        --
--=======================--

type Event = 
      Click Position
    | Drag Position
    | Drop Position
    | GUI (Msg Event)
    | Debug Bool

type alias UI =
    { mdl : Model
    , turn : String
    , debug : Bool
    }

