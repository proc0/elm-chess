module State.Move exposing (..)

import Matrix exposing (..)
import Char exposing (..)
import Debug exposing (..)

import Mouse exposing (..)

import Data.Type exposing (..)
import Data.Tool exposing (..)
import Model.FEN exposing (..)
import State.Rules exposing (..)

startDrag : Mouse.Position -> Square -> Square
startDrag ps sq = 
    case sq.piece of
            Just p -> 
                { sq 
                | point = ps
                , piece = Just ({ p | active = True })
                }
            Nothing -> sq

updateDrag : Mouse.Position -> Square -> Maybe Player -> Maybe Player
updateDrag xy sq player = 
    Maybe.map (\p -> 
        { p | drag = Just ({ sq | point = xy })
        }) player

validate : Square -> Board -> Board
validate sq bd =
    -- append input square as valid
    let newSquare = sq.piece
            |> Maybe.map (\pc -> { sq | piece = Just { pc | active = True }}) 
            |> Maybe.withDefault sq
        validSquares = newSquare::(getValidSquares sq bd)
        checkMoves sq_ b = 
            Matrix.update (toLocation sq_.point) 
                (\{ point, piece, valid } ->
                    Square point piece True) b 
    in List.foldl checkMoves bd validSquares

getValidSquares : Square -> Board -> List Square
getValidSquares sq bd = (flip filterSameSquares) bd <| getPossible sq bd

getPossible : Square -> Board -> List Square
getPossible square board = 
    case square.piece of
        Just pc -> List.map (flip moveSquare square) (pieceMoves square board)
        Nothing -> []

moveSquare : (Point -> Point) -> Square -> Square
moveSquare move sq = Square (move sq.point) sq.piece True

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
toSAN (sq1, sq2) =
        let toNotation sq = 
                    sq.piece |> Maybe.map (\p -> 
                            let ltr = figCharMap p.role
                            in if p.role /= Pawn
                                then String.fromChar (if p.color == White then toUpper ltr else ltr) ++ (String.fromChar <| fromCode (sq.point.x + 97)) ++ (toString <| 8-sq.point.y)
                                else (String.fromChar <| fromCode (sq.point.x + 97)) ++ (toString <| 8-sq.point.y)
                            ) 
                        |> Maybe.withDefault ""

            moveWhite = toNotation sq1
            moveBlack = sq2 |> Maybe.map toNotation |> Maybe.withDefault ""
        in moveWhite ++ " " ++ moveBlack