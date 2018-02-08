module State.Move exposing (..)

import Matrix exposing (..)
import Char exposing (..)
import Debug exposing (..)

import Mouse exposing (..)

import Data.Type exposing (..)
import Data.Tool exposing (..)
import Model.FEN exposing (..)
import State.Rules exposing (..)

startDrag : Mouse.Position -> Square -> Maybe Moving
startDrag ps sq = 
    sq.piece |> Maybe.map (\p -> Lift { p | point = ps })

--updateDrag : Mouse.Position -> Square -> Maybe Player -> Maybe Player
--updateDrag xy sq player = 
--    Maybe.map (\p -> 
--        { p | drag = Just ({ sq | point = xy })
--        }) player

validate : Move -> Board -> Board
validate move board =
    -- append input square as valid
    let pc = move.piece
        newSquare = Square move.start (Just pc) True True
        validSquares = newSquare::(getValidSquares move board)
        _ = log "valid" validSquares
        checkMoves sq bd = 
            Matrix.update (toLocation sq.point) 
                (\{ point, piece, valid, active } ->
                    Square point piece True active) bd 
    in List.foldl checkMoves board validSquares

getValidSquares : Move -> Board -> List Square
getValidSquares move board = (flip filterSameSquares) board <| getPossible move board

getPossible : Move -> Board -> List Square
getPossible move board = List.map (flip moveSquare move) (pieceMoves move board)

moveSquare : (Point -> Point) -> Move -> Square
moveSquare move mv = Square (move mv.start) (Just mv.piece) True False

filterSameSquares : List Square -> Board -> List Square
filterSameSquares squares bd =
    let filterSquare target =
            let square = Matrix.get (toLocation target.point) bd
            in case square of
                Just sq -> isSameColor sq target
                Nothing -> False
    in List.filter filterSquare squares

isSameColor : Square -> Square -> Bool
isSameColor s1 s2 = 
    let isWhite {color} =
            case color of
                White -> True
                Black -> False
        avoidWhite {color} =
            case color of
                White -> False
                Black -> True
        avoidBlack {color} =
            case color of
                White -> True
                Black -> False
        checkRule p1 p2 =
            if isWhite p1
            then avoidWhite p2
            else avoidBlack p2
        valid = Maybe.map2 checkRule s1.piece s2.piece 
    in case valid of
        Just v -> v
        Nothing -> True

toSAN : Move -> String
toSAN move =
        let toNotation mv = 
            let ltr = figCharMap mv.piece.role
            in if mv.piece.role /= Pawn
               then String.fromChar (if mv.piece.color == White then toUpper ltr else ltr) ++ (String.fromChar <| fromCode (mv.start.x + 97)) ++ (toString <| 8-mv.start.y)
               else (String.fromChar <| fromCode (mv.start.x + 97)) ++ (toString <| 8-mv.start.y)
        in toNotation move

