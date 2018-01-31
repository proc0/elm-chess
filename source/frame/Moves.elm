module Frame.Moves exposing (..)

import Matrix exposing (..)
import Debug exposing (..)

import Data.Main exposing (..)
import Data.Game as Game exposing (..)
import Frame.Movement exposing (..)
import Settings exposing (..)
import Toolkit exposing (..)

validate : Square -> Board -> Board
validate sq bd =
    -- append input square as valid
    let validSquares = sq::(getValidSquares sq bd)
        checkMoves sq_ b = 
            Matrix.update (toLocation sq_.position) 
                (\{ position, piece, valid } ->
                    Square position piece True) b 
    in List.foldl checkMoves bd validSquares

getValidSquares : Square -> Board -> List Square
getValidSquares sq bd = (flip filterSameSquares) bd <| getPossible sq bd

filterSameSquares : List Square -> Board -> List Square
filterSameSquares squares bd =
    let filterSquare target =
            let square = Matrix.get (toLocation target.position) bd
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


getPossible : Square -> Board -> List Square
getPossible square board = 
    case square.piece of
        Just pc -> List.map (flip moveSquare square) (pieceMoves square board)
        Nothing -> []

moveSquare : (Position -> Position) -> Square -> Square
moveSquare move sq = Square (move sq.position) sq.piece True

pieceMoves : Square -> Board -> List (Position -> Position)
pieceMoves square board = 
    let ps = square.position
        getCardinals p = cardinals board p
        getDiagonals p = diagonals board p
        moves role =
            case role of
                Pawn    -> pawnMoves square board
                Bishop  -> getDiagonals ps
                Rook    -> getCardinals ps 
                Queen   -> List.append (getDiagonals ps) (getCardinals ps)                
                Knight -> 
                    [ up 2 >> right 1
                    , up 2 >> left 1
                    , down 2  >> left 1
                    , down 2  >> right 1
                    , left 2  >> up 1
                    , left 2  >> down 1
                    , right 2 >> up 1
                    , right 2 >> down 1
                    ]
                King ->
                    [ up 1
                    , down 1
                    , left 1
                    , right 1
                    , up 1 >> left 1
                    , up 1 >> right 1
                    , down 1 >> left 1
                    , down 1 >> right 1
                    ]
                _ -> []
    in case square.piece of
        Just ({color, role} as p) ->
            case color of 
                White -> moves role
                Black -> moves role
        Nothing -> []