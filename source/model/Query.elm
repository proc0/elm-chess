module Model.Query exposing (..)

import Maybe.Extra exposing (isJust)
import List exposing (head, any)

import Data.Type exposing (..)
import Data.Tool exposing (..)
import Model.Moves exposing (..)

-- general queries
--================--

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

isCastling : Piece -> Bool
isCastling piece =
    let castlesLocations = 
            toLocations 
               ([ (7,6)
                , (7,2)
                , (0,6)
                , (0,2)
                ])
    in
    case piece.role of
        King -> any ((==) piece.location) castlesLocations
        _ -> False

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

isUntouched : Square -> Bool
isUntouched square =
    case square.piece of
        Just piece -> piece.tick == 1
        _ -> False

hasPawn : Square -> Bool
hasPawn square =
    case square.piece of
        Just piece ->
            case piece.role of
                Pawn -> True
                _ -> False
        _ -> False

