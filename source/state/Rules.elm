module State.Rules exposing (..)

import Array exposing (..)
import Matrix exposing (..)
import Debug exposing (..)

import Data.Type exposing (..)
import Data.Tool exposing (..)

pieceMoves : Move -> Board -> List (Point -> Point)
pieceMoves move board = 
    let ps = move.start
        getParallels = parallels board
        getDiagonals = diagonals board
        moves role =
            case role of
                Pawn    -> pawnMoves move board
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
    in case move.piece.color of 
        White -> moves move.piece.role
        Black -> moves move.piece.role

pawnMoves : Move -> Board -> List (Point -> Point)
pawnMoves move board = 
    let pawnMove =
            case move.piece.color of
                White -> [ up 1 ]
                Black -> [ down 1 ]
        -- pawn potential moves:
        -- forward move (two if first move),
        -- capture moves if a piece is there              
        totalMoves = 
            Maybe.map (\mv -> 
                (pawnFirstMove mv) 
                ++ (pawnCaptures mv board) 
                ++ pawnMove
            ) (Just move)
    in Maybe.withDefault [] totalMoves

pawnFirstMove : Move -> List (Point -> Point)
pawnFirstMove mv =
    -- check rank and add two steps
    let whiteMove pt =
            if pt.y == 6
            then Just (up 2)
            else Nothing
        blackMove pt =
            if pt.y == 1
            then Just (down 2)
            else Nothing
    -- wrap and perform respective check
    in List.concatMap (\pc ->
                List.filterMap (\{color, point} -> 
                    case color of
                        White -> whiteMove point
                        Black -> blackMove point
                    ) [pc]
        ) [mv.piece]

pawnCaptures : Move -> Board -> List (Point -> Point)
pawnCaptures mv bd =
        let position = mv.start
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
            checkSquare moves =
                let target = Matrix.get (toLocation <| moves position) bd
                    posons = Maybe.map (\t -> Maybe.map (\_ -> moves) t.piece) target
                in Maybe.withDefault (Just idle) posons
        in List.filterMap checkSquare (eats mv.piece.color)
                   
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
            in Maybe.withDefault (Piece White Zebra point False) sq.piece

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
            in Maybe.withDefault (Piece White Zebra point False) sq.piece

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
        emptySquare = Square pos Nothing False False
    in case sq of
            Just s -> s
            Nothing -> emptySquare

