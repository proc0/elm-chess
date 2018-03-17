module Model.Moves exposing (..)

import Matrix exposing (Location, loc, get)
import List exposing (foldl, any, map, concatMap, filterMap, length, head)
import Debug exposing (log)

import Data.Type exposing (..)
import Data.Tool exposing (..)

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

findSquare : Location -> Board -> Square
findSquare lc board = 
    let sq = Matrix.get lc board
    in 
    case sq of
        Just s -> s
        Nothing -> vacantSquare

