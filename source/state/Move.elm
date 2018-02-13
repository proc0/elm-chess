module State.Move exposing (..)

import Matrix exposing (..)
import Char exposing (..)
import Debug exposing (..)

import Mouse exposing (..)

import Data.Type exposing (..)
import Data.Tool exposing (..)
import Model.FEN exposing (..)
import State.Rules exposing (..)

--startDrag : Mouse.Position -> Piece -> Piece
--startDrag xy pc = { pc | position = xy }
--    let lc = (toLocation ps)
--    in sq.piece 
--        |> Maybe.map (\p -> Move p lc lc Nothing)
--        |> Maybe.withDefault idleMove

updatePiecePos : Mouse.Position -> Maybe Piece -> Maybe Piece
updatePiecePos xy pc = Maybe.map (\p -> { p | position = xy }) pc

validate : Move -> Board -> Board
validate move board =
    -- append input square as valid
    let pc = move.piece
        validLocations = move.start::(List.map (\t -> t move.start) (pieceMoves move board))
        --newSquare = Square move.start (Just pc) True True
        --validSquares = newSquare::(getValidSquares move board)
        _ = log "valid" validLocations
        checkMoves lc bd = 
            Matrix.update lc (\sq -> { sq | valid = True}) bd 
    in List.foldl checkMoves board validLocations

--getValidSquares : Move -> Board -> List Square
--getValidSquares move board = (flip filterSameSquares) board <| getPossible move board

--getPossible : Move -> Board -> List Square
--getPossible move board = List.map (flip moveSquare move) (pieceMoves move board)

--moveSquare : (Location -> Location) -> Move -> Square
--moveSquare move mv = 
--    let pc = mv.piece
--        newLocation = move mv.start
--    in Square newLocation (Just { pc | position = toBoardPosition newLocation }) True False

filterSameSquares : List Square -> Board -> List Square
filterSameSquares squares bd =
    let filterSquare target =
            let square = Matrix.get target.location bd
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
                (y1,x1) = mv.start
                --(x2,y2) = mv.end
            in if mv.piece.role /= Pawn
               then String.fromChar (if mv.piece.color == White then toUpper ltr else ltr) ++ (String.fromChar <| fromCode (x1 + 97)) ++ (toString <| 8-y1)
               else (String.fromChar <| fromCode (x1 + 97)) ++ (toString <| 8-y1)
        in toNotation move

