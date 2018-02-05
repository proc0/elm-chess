module State.Rules exposing (..)

import Array exposing (..)
import Matrix exposing (..)
import Debug exposing (..)

import Data.Type exposing (..)
import Data.Tool exposing (..)

pieceMoves : Square -> Board -> List (Point -> Point)
pieceMoves square board = 
    let ps = square.point
        getParallels = parallels board
        getDiagonals = diagonals board
        moves role =
            case role of
                Pawn    -> pawnMoves square board
                Bishop  -> getDiagonals ps
                Rook    -> getParallels ps 
                Queen   -> List.append (getDiagonals ps) (getParallels ps)                
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

pawnMoves : Square -> Board -> List (Point -> Point)
pawnMoves square board = 
    let pawnMove = case square.piece of
            Just {color} ->
                case color of
                    White -> [ up 1 ]
                    Black -> [ down 1 ]
            Nothing -> []
        -- pawn potential moves:
        -- forward square (two if first move),
        -- capture squares if a piece is there              
        totalMoves = 
            Maybe.map (\sq -> 
                (pawnFirstMove sq) 
                ++ (pawnCaptures sq board) 
                ++ pawnMove
            ) (Just square)
    in Maybe.withDefault [] totalMoves

pawnFirstMove : Square -> List (Point -> Point)
pawnFirstMove sq =
    -- check rank and add two steps
    let whiteMove s =
            if s.point.y == 6
            then Just (up 2)
            else Nothing
        blackMove s =
            if sq.point.y == 1
            then Just (down 2)
            else Nothing
    -- wrap and perform respective check
    in List.concatMap (\pc ->
            let pawnWrap = case pc of
                Just pn -> [pn]
                Nothing -> []
            in List.filterMap (\{color} -> 
                case color of
                    White -> whiteMove sq
                    Black -> blackMove sq
                ) pawnWrap
        ) [sq.piece]

pawnCaptures : Square -> Board -> List (Point -> Point)
pawnCaptures sq bd =
        let ps = sq.point
            eats color = 
                case color of
                    White ->
                        [ up 1 >> left 1
                        , up 1 >> right 1
                        ]
                    Black ->
                        [ down 1 >> left 1
                        , down 1 >> right 1
                        ]
            checkSquare mv =
                let target = Matrix.get (toLocation <| mv ps) bd
                    posons = Maybe.map (\t -> Maybe.map (\_ -> mv) t.piece) target
                in Maybe.withDefault (Just idle) posons
        in case sq.piece of
            Just {color} -> List.filterMap checkSquare (eats color)
            Nothing -> []
                   
-- pedantic
idle : Point -> Point
idle p = p

up : Int -> Point -> Point
up n p = 
    { x = p.x
    , y = p.y - n
    }

down : Int -> Point -> Point
down n p = 
    { x = p.x
    , y = p.y + n
    }

left : Int -> Point -> Point
left n p = 
    { x = p.x - n
    , y = p.y 
    }

right   : Int -> Point -> Point
right n p = 
    { x = p.x + n
    , y = p.y
    }

diagonals : Board -> Point -> List (Point -> Point)
diagonals board point = 
    let directions = 
            [ (up, left)
            , (down, left)
            , (up, right)
            , (down, right)
            ]
        stepRange = List.map ((+) 1) boardside
        curPiece = 
            let sq = findSquare point board
            in Maybe.withDefault (Piece White Zebra False False) sq.piece

        step = List.foldl 
            (\(d1,d2) (m, c) -> 
                List.foldl (\i (memo, cont) -> 
                    let nextStep = (d1 i) >> (d2 i)
                    -- stop processing if piece found
                    in if cont
                        then 
                            let blocking = findSquare (nextStep point) board
                            in case blocking.piece of
                                Just {color} -> 
                                    if color /= curPiece.color
                                    then (nextStep::memo, False)
                                    else (memo, False)
                                Nothing -> (nextStep::memo, True)
                        else (memo, False)
                    ) (m, True) stepRange) ([],True)
    in fst <| step directions

parallels : Board -> Point -> List (Point -> Point)
parallels board point =
    let directions = 
            [ up
            , right
            , down
            , left
            ]
        stepRange = List.map ((+) 1) boardside
        curPiece = 
            let sq = findSquare point board
            in Maybe.withDefault (Piece White Zebra False False) sq.piece

        step = List.foldl 
            (\d (m, c) -> 
                List.foldl (\i (memo, cont) -> 
                        if cont
                        then 
                            let blocking = findSquare (d i point) board
                            in case blocking.piece of
                                Just {color} ->
                                    if color /= curPiece.color
                                    then ((d i)::memo, False)
                                    else (memo, False)
                                Nothing -> ((d i)::memo, True)                            
                        else (memo, False)
                        ) (m, True) stepRange) ([],True)
    in fst <| step directions

findSquare : Point -> Board -> Square
findSquare pos board = 
    let sq = Matrix.get (toLocation pos) board
        emptySquare = Square pos Nothing False
    in case sq of
            Just s -> s
            Nothing -> emptySquare

