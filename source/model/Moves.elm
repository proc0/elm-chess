module Model.Moves exposing (..)

import Matrix exposing (Location, loc, get)
import List exposing (foldl, map, concatMap, filterMap, length, head)
import Debug exposing (log)
import Maybe.Extra exposing (..)

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

isEnPassant : Board -> Piece -> Bool
isEnPassant board piece = 
    passanting piece && (isJust << head <| enPassant board piece)

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
                    ++
                    find castle
                _ -> []
    in 
    -- get possible moves by role
    moves piece.role 
    -- and filter squares occupied
    -- by same color pieces
    |> distinct piece board

--kingside : Color -> List Location
--kingside color =
--    toLocations (case color of
--        White -> [(7,5),(7,6),(7,7)]
--        Black -> [(0,5),(0,6),(0,7)])

--queenside : Color -> List Location
--queenside color =
--    toLocations (case color of
--        White -> [(7,0),(7,1),(7,2),(7,3)]
--        Black -> [(0,0),(0,1),(0,2),(0,3)])

applyRule : Board -> Piece -> (Translation, Square -> Bool) -> List Translation -> List Translation
applyRule board piece (move, rule) moves =
    let target = Matrix.get (move piece.location) board
    in 
    (target |> Maybe.map 
        (\square ->
            if rule square
            then move::moves
            else moves)) ? moves

castle : Board -> Piece -> List Translation
castle board king = 
    let withKing fn =
            fn board king
        clearKing = 
            foldl (withKing applyRule) []
        sides = 
            [ right
            , left
            ]
        clear tests =
            (length <| clearKing tests)
            ==
            (length tests)
        castling step =
            let (y,x) = 
                    step 4 king.location
                rookLoc =
                    if y /= 0
                    then 3
                    else 4
                newRook = 
                    [ (step rookLoc, isNewRook)
                    ]
                jumpSquares =
                    [ (step 1, isVacant)
                    , (step 2, isVacant)
                    ]
            in 
            if clear newRook && clear jumpSquares
            then map fst jumpSquares
            else []
    in
    if stationary king
    then concatMap castling sides
    else []


pawnMoves : Board -> Piece -> List Translation
pawnMoves board pawn =
    let (y,x) = pawn.location
        step = forward pawn
        checkPawn fn = 
            fn board pawn
        checkRules = 
            foldl (checkPawn applyRule) []
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
    checkRules rules ++ checkPawn enPassant

enPassant : Board -> Piece -> List Translation
enPassant board pawn = 
    let step = forward pawn
        checkPawn = foldl (applyRule board pawn) []
        passantRules move = 
            [ (move 1, isFirstMove)
            , (move 1, isPawn)
            ]
        passing : Movement -> Square -> Bool
        passing dir square =
            if isVacant square
            then (length << checkPawn <| passantRules dir)
                 == -- rules length does not change after check
                 (length <| passantRules identity)
            else False
        rules : List (Translation, Square -> Bool)
        rules =
            [ (step 1 >> left 1, passing left)
            , (step 1 >> right 1, passing right)
            ]
        --_ = log "checkPassant" <| checkPawn rules
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
        stepRange = map ((+) 1) boardside
        search = foldl 
            (\(d1,d2) (m, c) -> 
                foldl (\i (memo, cont) -> 
                    let nextStep = (d1 i) >> (d2 i)
                    -- stop processing if piece found
                    in if cont
                        then 
                            let blocking = findSquare (nextStep point) board
                            in 
                            case blocking.piece of
                                Just {color} -> 
                                    if color /= piece.color
                                    then (nextStep::memo, False)
                                    else (memo, False)
                                Nothing -> (nextStep::memo, True)
                        else (memo, False)
                    ) (m, True) stepRange) ([],True)
    in fst <| search directions

parallels : Board -> Piece -> List Translation
parallels board piece =
    let point = piece.location
        directions = 
            [ up
            , right
            , down
            , left
            ]
        stepRange = map ((+) 1) boardside
        search = foldl 
            (\d (m, c) -> 
                foldl (\i (memo, cont) -> 
                    if cont
                    then 
                        let blocking = findSquare (d i point) board
                        in case blocking.piece of
                            Just {color} ->
                                if color /= piece.color
                                then ((d i)::memo, False)
                                else (memo, False)
                            Nothing -> ((d i)::memo, True)                            
                    else (memo, False)
                    ) (m, True) stepRange) ([],True)
    in fst <| search directions

-- TODO: refactor and take out Rules, flip type
distinct : Piece -> Board -> List Translation -> List Translation
distinct piece board locations = 
    filterMap (\move -> 
        case Matrix.get (move piece.location) board of
            Just square ->
                case square.piece of
                    Just pc -> 
                        if pc.color == piece.color
                        then Nothing
                        else Just move
                    _ -> Just move
            _ -> Nothing) locations

findSquare : Location -> Board -> Square
findSquare lc board = 
    let sq = Matrix.get lc board
    in 
    case sq of
        Just s -> s
        Nothing -> emptySquare

