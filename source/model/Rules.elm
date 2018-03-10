module Model.Rules exposing (..)

import Array exposing (..)
import Matrix exposing (..)
import Debug exposing (..)
import Maybe.Extra as Maebe exposing (..)

import Data.Type exposing (..)
import Data.Tool exposing (..)

-- rules
-- =============
isOccupied : Square -> Bool
isOccupied square = isJust square.piece

isVacant : Square -> Bool
isVacant = not << isOccupied

isFirstMove : Square -> Bool
isFirstMove square =
    case square.piece of
        Just piece -> piece.ellapsed == 1
        _ -> False

isPawn : Square -> Bool
isPawn square =
    case square.piece of
        Just piece ->
            case piece.role of
                Pawn -> True
                _ -> False
        _ -> False

starting : Piece -> Bool
starting piece = 
    let (y,x) = 
        piece.location
    in 
    case piece.color of
        White -> y == 6
        Black -> y == 1

passanting : Piece -> Bool
passanting pawn =
    let (y,x) =
        pawn.location
    in 
    case pawn.color of
        White -> y == 3
        Black -> y == 4
