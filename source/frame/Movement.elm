module Frame.Movement exposing (..)

import Array exposing (..)
import Debug exposing (..)

import Data.Main exposing (..)
import Data.Game as Game exposing (..)
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
    let moves p =
        case p of
            Pawn -> 
                let isFirstMove = pawnFirstMove piece position
                    pawnEats = pawnCanEat piece position board
                    firstMove = case piece of
                        White pc ->
                            case isFirstMove of
                                Just mv -> mv::[ up 1 ]
                                Nothing -> [ up 1 ]
                        Black pc ->
                            case isFirstMove of
                                Just mv -> mv::[ down 1 ]
                                Nothing -> [ down 1 ]
                in firstMove ++ pawnEats
            Knight -> 
                [ up 2 >> right 1
                , up 2 >> left 1
                , down 2 >> left 1
                , down 2 >> right 1
                , left 2 >> up 1
                , left 2 >> down 1
                , right 2 >> up 1
                , right 2 >> down 1
                ]
            Rook    -> cardinals board position 
            Bishop  -> diagonals board position
            Queen   -> List.append 
                        (diagonals board position) 
                        (cardinals board position)
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

diagonals : Board -> Position -> List (Position -> Position)
diagonals board position = 
    --[up 1 >> left 1, up 2 >> left 2, up 3 >> left 3, up 4 >> left 4]
    let directions = 
            [ (up, left)
            , (down, left)
            , (up, right)
            , (down, right)
            ]
        stepRange = List.map ((+) 1) boardside
        step = List.foldl 
                (\(d1,d2) (m, c) -> 
                    List.foldl (\i (memo, cont) -> 
                        let nextStep = (d1 i) >> (d2 i)
                        -- stop processing if piece found
                        in if cont
                            then 
                                let blocking = findSquare (nextStep position) board
                                in case blocking.piece of
                                    Just p -> (memo, False)
                                    Nothing -> (nextStep::memo, True)
                            else (memo, False)
                        ) (m, True) stepRange) ([],True)
    in fst <| step directions

cardinals : Board -> Position -> List (Position -> Position)
cardinals board position =
    let directions = 
            [ up
            , right
            , down
            , left
            ]
        stepRange = List.map ((+) 1) boardside
        step = List.foldl 
            (\d (m, c) -> 
                List.foldl (\i (memo, cont) -> 
                        if cont
                        then 
                            let blocking = findSquare (d i position) board
                            in case blocking.piece of
                                Just p -> (memo, False)
                                Nothing -> ((d i)::memo, True)                            
                        else (memo, False)
                        ) (m, True) stepRange) ([],True)
    in fst <| step directions    

up : Int -> Position -> Position
up n p = {x=p.x, y=p.y-n}

down : Int -> Position -> Position
down n p = {x=p.x, y=p.y+n}

left : Int -> Position -> Position
left n p = {x=p.x-n, y=p.y}

right : Int -> Position -> Position
right n p = {x=p.x+n, y=p.y}

findSquare : Game.Position -> Game.Board -> Square
findSquare pos board = 
    let emptySquare = Square pos Nothing False
        retrieve : Int -> List a -> Maybe a
        retrieve index items = 
                items 
                |> Array.fromList 
                |> Array.get index
        getSquare b = 
            retrieve pos.y b 
            |> Maybe.andThen (retrieve pos.x)

    in case getSquare board of
            Just match -> match
            Nothing -> emptySquare

pawnFirstMove : Piece -> Position -> Maybe (Position -> Position)
pawnFirstMove pc ps =
    case pc of
        White _ -> 
            if ps.y == 6
            then Just (up 2)
            else Nothing
        Black _ ->
            if ps.y == 1
            then Just (down 2)
            else Nothing 

pawnCanEat : Piece -> Position -> Board -> List (Position -> Position)
pawnCanEat pc ps bd =
        let topLeft = {x=ps.x-1,y=ps.y-1} 
            topRight = {x=ps.x+1,y=ps.y-1} 
            bottomLeft = {x=ps.x-1,y=ps.y+1} 
            bottomRight = {x=ps.x+1,y=ps.y+1} 
        in case pc of
            White _ -> 
                let rank = bd |> Array.fromList |> Array.get (ps.y-1)
                in case rank of
                    Just r -> List.filterMap (\sq -> 
                                    if sq.position == topLeft
                                    then case sq.piece of
                                        Just p -> Just (up 1 >> left 1)
                                        Nothing -> Nothing
                                    else if sq.position == topRight
                                    then case sq.piece of
                                        Just p -> Just (up 1 >> right 1)
                                        Nothing -> Nothing                                    
                                    else Nothing
                    ) r
                    Nothing -> []
            Black _ ->
                let rank = bd |> Array.fromList |> Array.get (ps.y+1)
                in case rank of
                    Just r -> List.filterMap (\sq -> 
                                    if sq.position == bottomLeft
                                    then case sq.piece of
                                        Just p -> Just (down 1 >> left 1)
                                        Nothing -> Nothing
                                    else if sq.position == bottomRight
                                    then case sq.piece of
                                        Just p -> Just (down 1 >> right 1)
                                        Nothing -> Nothing                                    
                                    else Nothing
                    ) r
                    Nothing -> []