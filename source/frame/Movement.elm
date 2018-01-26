module Frame.Movement exposing (..)

import Array exposing (..)
import Matrix exposing (..)
import Debug exposing (..)

import Data.Main exposing (..)
import Data.Game as Game exposing (..)
import Settings exposing (..)
import Toolkit exposing (..)

-- pedantic
idle : Position -> Position
idle p = p

up : Int -> Position -> Position
up n p = 
    { x = p.x
    , y = p.y - n
    }

down : Int -> Position -> Position
down n p = 
    { x = p.x
    , y = p.y + n
    }

left : Int -> Position -> Position
left n p = 
    { x = p.x - n
    , y = p.y 
    }

right : Int -> Position -> Position
right n p = 
    { x = p.x + n
    , y = p.y 
    }

fromPos : Position -> Location
fromPos p = loc p.x p.y

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


findSquare : Game.Position -> Game.Board -> Square
findSquare pos board = 
    let sq = Matrix.get (loc pos.x pos.y) board
        emptySquare = Square pos Nothing False
    in case sq of
            Just s -> s
            Nothing -> emptySquare
    --let emptySquare = Square pos Nothing False
    --    retrieve : Int -> List a -> Maybe a
    --    retrieve index items = 
    --            items 
    --            |> Array.fromList 
    --            |> Array.get index
    --    getSquare b = 
    --        retrieve pos.y b 
    --        |> Maybe.andThen (retrieve pos.x)

    --in case getSquare board of
    --        Just match -> match
    --        Nothing -> emptySquare

pawnMoves : Piece -> Position -> Board -> List (Position -> Position)
pawnMoves piece position board = 
    let location = (loc position.x position.y)
        pawnSquare = Matrix.get location board
        pawnMove = case piece of
            White _ -> [ up 1 ]
            Black _ -> [ down 1 ] 
        -- pawn potential moves:
        -- forward square (two if first move),
        -- capture squares if a piece is there              
        totalMoves = 
            Maybe.map (\sq -> 
                (pawnFirstMove sq) 
                ++ (pawnCaptures sq board) 
                ++ pawnMove
            ) pawnSquare
    in Maybe.withDefault [] totalMoves

pawnFirstMove : Square -> List (Position -> Position)
pawnFirstMove sq =
    -- check rank and add two steps
    let whiteMove s =
            if s.position.y == 6
            then Just (up 2)
            else Nothing
        blackMove s =
            if sq.position.y == 1
            then Just (down 2)
            else Nothing
    -- wrap and perform respective check
    in List.concatMap (\pc ->
            let pawnWrap = case pc of
                Just pn -> [pn]
                Nothing -> []
            in List.filterMap (\p -> 
                case p of
                    White _ -> whiteMove sq
                    Black _ -> blackMove sq
                ) pawnWrap
        ) [sq.piece]

pawnCaptures : Square -> Board -> List (Position -> Position)
pawnCaptures sq bd =
        let ps = sq.position
            whiteEats = 
                [ up 1 >> left 1
                , up 1 >> right 1
                ]
            blackEats = 
                [ down 1 >> left 1
                , down 1 >> right 1
                ]
            checkSquare mv =
                let target = Matrix.get (fromPos <| mv ps) bd
                    posons = Maybe.map (\t -> Maybe.map (\_ -> mv) t.piece) target
                in Maybe.withDefault (Just idle) posons
        in case sq.piece of
            Just pc ->
                case pc of
                    White _ -> List.filterMap checkSquare whiteEats
                    Black _ -> List.filterMap checkSquare blackEats
            Nothing -> []
