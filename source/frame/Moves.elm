module Frame.Moves exposing (..)

import Matrix exposing (..)
import Debug exposing (..)

import Data.Main exposing (..)
import Data.Game as Game exposing (..)
import Frame.Movement exposing (..)
import Settings exposing (..)
import Toolkit exposing (..)

getPossible : Square -> Board -> List Square
getPossible square board = 
    case square.piece of
        Just pc -> List.map (flip moveSquare square) (pieceMoves pc square.position board)
        Nothing -> [square]

moveSquare : (Position -> Position) -> Square -> Square
moveSquare move sq = Square (move sq.position) sq.piece True

pieceMoves : Piece -> Position -> Board -> List (Position -> Position)
pieceMoves piece position board = 
    let ps = position
        getCardinals p = cardinals board p
        getDiagonals p = diagonals board p
        moves p =
            case p of
                Pawn    -> pawnMoves piece ps board
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
    in case piece of
        White pc -> moves pc
        Black pc -> moves pc