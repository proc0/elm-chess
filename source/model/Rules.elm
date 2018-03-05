module Model.Rules exposing (..)

import Array exposing (..)
import Matrix exposing (..)
import Debug exposing (..)
import Maybe.Extra as Maebe exposing (..)

import Data.Type exposing (..)
import Data.Tool exposing (..)
             
-- pedantic
stay : Location -> Location
stay l = l

-- Location y is reversed
up : Int -> Location -> Location
up n (y,x) = loc (y - n) x

down : Int -> Location -> Location
down n (y,x) = loc (y + n) x

left : Int -> Location -> Location
left n (y,x) = loc y (x - n)

right : Int -> Location -> Location
right n (y,x) = loc y (x + n)

pieceMoves : Piece -> Board -> List (Location -> Location)
pieceMoves piece board = 
    let location = toBoardLocation piece.position
        find f = f board location
        getMove role =
            case role of
                Pawn    -> movePawn piece board
                Bishop  -> find diagonals
                Rook    -> find parallels
                Queen   -> find diagonals ++ find parallels               
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
    in getMove piece.role |> filterSameColor piece board

movePawn : Piece -> Board -> List (Location -> Location)
movePawn piece board =
        let ((y,x) as location) = 
                toBoardLocation piece.position
            step = 
                case piece.color of
                    White -> up
                    Black -> down
            startLocation = 
                case piece.color of
                    White -> 6
                    Black -> 1
            if_occupied sq =
                case sq.piece of
                    Just p -> True
                    Nothing -> False
            if_empty sq = 
                case sq.piece of
                    Just p -> False
                    Nothing -> True
            possibleMoves = 
                -- if pawn is hasn't moved
                (if y == startLocation
                -- add two steps if target empty
                then [(step 2, if_empty)] 
                else []) 
                ++ 
                [ (step 1, if_empty)
                -- eat diagonals if occupied
                , (step 1 >> left 1, if_occupied)
                , (step 1 >> right 1, if_occupied)
                ]
            applyRule (move, rule) moves =
                let target = Matrix.get (move location) board
                in 
                (target |> Maybe.map 
                    (\square ->
                        if rule square
                        then move::moves
                        else moves)) ? moves
        in List.foldl applyRule [] possibleMoves

diagonals : Board -> Location -> List (Location -> Location)
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
            in Maybe.withDefault nullPiece sq.piece

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

parallels : Board -> Location -> List (Location -> Location)
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
            in Maybe.withDefault nullPiece sq.piece

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

filterSameColor : Piece -> Board -> List (Location -> Location) -> List (Location -> Location)
filterSameColor piece board locations = 
    List.filterMap (\move -> 
        let sq = findSquare (move <| toBoardLocation piece.position) board
        in 
        case sq.piece of
            Just p -> 
                if p.color == piece.color
                then Nothing
                else Just move
            _ -> Just move) locations

findSquare : Location -> Board -> Square
findSquare lc board = 
    let sq = Matrix.get lc board
    in case sq of
            Just s -> s
            Nothing -> emptySquare

--stay : Position -> Position
--stay p = p

--up : Int -> Position -> Position
--up n p = 
--    { x = p.x
--    , y = p.y - n
--    }

--down : Int -> Position -> Position
--down n p = 
--    { x = p.x
--    , y = p.y + n
--    }

--left : Int -> Position -> Position
--left n p = 
--    { x = p.x - n
--    , y = p.y 
--    }

--right   : Int -> Position -> Position
--right n p = 
--    { x = p.x + n
--    , y = p.y
--    }
