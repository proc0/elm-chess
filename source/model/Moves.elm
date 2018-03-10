module Model.Moves exposing (..)

import Array exposing (..)
import Matrix exposing (..)
import Debug exposing (..)
import Maybe.Extra as Maebe exposing (..)

import Data.Type exposing (..)
import Data.Tool exposing (..)
import Model.Rules exposing (..)

-- board translations
-- ==================
stay : Translation
stay l = l

-- Location y is reversed
up : Int -> Translation
up n (y,x) = loc (y - n) x

down : Int -> Translation
down n (y,x) = loc (y + n) x

left : Int -> Translation
left n (y,x) = loc y (x - n)

right : Int -> Translation
right n (y,x) = loc y (x + n)

forward : Piece -> Int -> Translation
forward piece = 
    case piece.color of
        White -> up
        Black -> down

backward : Piece -> Int -> Translation
backward piece = 
    case piece.color of
        White -> down
        Black -> up

enPassant : Board -> Piece -> Bool
enPassant board piece = 
    isJust << List.head <| passant board piece

pieceMoves : Piece -> Board -> List Translation
pieceMoves piece board = 
    let find f = f board piece
        moves role =
            case role of
                Pawn    -> find pawnMoves
                Bishop  -> find diagonals
                Rook    -> find parallels
                Queen   -> find diagonals 
                           ++ 
                           find parallels               
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
    in 
    -- get possible moves by role
    moves piece.role 
    -- and filter squares occupied
    -- by same color pieces
    |> distinct piece board

pawnMoves : Board -> Piece -> List Translation
pawnMoves board pawn =
        let (y,x) = pawn.location
            step = forward pawn
            checkPawn = List.foldl <| applyRule board pawn
            checkPassant = 
                if passanting pawn
                then passant board pawn
                else []
            rules : List (Translation, Square -> Bool)
            rules = 
                -- if pawn hasn't moved
                (if starting pawn
                -- add two steps if vacant
                then [(step 2, isVacant)] 
                else []) 
                ++ 
                -- can step forward if vacant
                [ (step 1, isVacant)
                -- can eat diagonals if occupied
                , (step 1 >> left 1, isOccupied)
                , (step 1 >> right 1, isOccupied)
                ]
        in 
        checkPawn checkPassant rules

passant : Board -> Piece -> List Translation
passant board pawn = 
    let step = forward pawn
        checkPawn = List.foldl (applyRule board pawn) []
        passantRules move = 
            [ (move 1, isFirstMove)
            , (move 1, isPawn)
            ]
        passing : Movement -> Square -> Bool
        passing dir square =
            if isVacant square
            then (List.length << checkPawn <| passantRules dir)
                 == -- rules length does not change after check
                 (List.length <| passantRules identity)
            else False -- some rule(s) failed
        rules : List (Translation, Square -> Bool)
        rules =
            [ (step 1 >> left 1, passing left)
            , (step 1 >> right 1, passing right)
            ]
    in 
    checkPawn rules

diagonals : Board -> Piece -> List Translation
diagonals board piece = 
    let point = piece.location
        directions = 
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

parallels : Board -> Piece -> List Translation
parallels board piece =
    let point = piece.location
        directions = 
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

-- TODO: refactor and take out Rule, flip type
distinct : Piece -> Board -> List Translation -> List Translation
distinct piece board locations = 
    List.filterMap (\move -> 
        let sq = findSquare (move piece.location) board
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
    in 
    case sq of
        Just s -> s
        Nothing -> emptySquare

applyRule : Board -> Piece -> (Translation, Square -> Bool) -> List Translation -> List Translation
applyRule board piece (move, rule) moves =
    let target = Matrix.get (move piece.location) board
    in 
    (target |> Maybe.map 
        (\square ->
            if rule square
            then move::moves
            else moves)) ? moves
