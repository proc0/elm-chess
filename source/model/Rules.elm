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

isNewRook : Square -> Bool
isNewRook square =
    case square.piece of
        Just pc ->
            case pc.role of
                Rook -> 
                    pc.tick == 0
                _ -> False
        _ -> False


isFirstMove : Square -> Bool
isFirstMove square =
    case square.piece of
        Just piece -> piece.tick == 1
        _ -> False

isPawn : Square -> Bool
isPawn square =
    case square.piece of
        Just piece ->
            case piece.role of
                Pawn -> True
                _ -> False
        _ -> False

startingRank : Piece -> Int
startingRank piece =
    let offset n = 
        case piece.color of
            White -> 7 - n
            Black -> n
    in
    case piece.role of
        Pawn -> offset 1
        _ -> offset 0

starting : Piece -> Bool
starting piece = 
    let (y,x) = 
        piece.location
    in 
    startingRank piece == y

passanting : Piece -> Bool
passanting pawn =
    let (y,x) =
        pawn.location
    in 
    case pawn.color of
        White -> y == 3
        Black -> y == 4

stationary : Piece -> Bool
stationary piece = 
    piece.tick == 0
    
