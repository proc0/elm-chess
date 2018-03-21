module Model.Moves exposing (..)

import Matrix exposing (Location, loc, get)
import List exposing (foldl, any, map, concat, concatMap, filterMap, length, head, reverse, singleton)
import Maybe.Extra exposing ((?))
import Debug exposing (log)

import Data.Tool exposing (..)
import Data.Type exposing (..)
import Data.Query exposing (..)

-- board translations
-- ==================
stay : Translation
stay l = l

-- Location y is reversed
up : Movement
up n (y,x) = loc (y - n) x

down : Movement
down n (y,x) = loc (y + n) x

left : Movement
left n (y,x) = loc y (x - n)

right : Movement
right n (y,x) = loc y (x + n)

cardinals : (Movements, Movements)
cardinals = ([up, down], [left, right])
        {-
↖ ↑ ↗   all possible movements,    
←   →   asterisk shaped arrows
↙ ↓ ↘   -}
asterisk : List Movements
asterisk = 
    let combinatorial = 
        (uncurry <| liftAp (++)) << mapBoth (($>>) singleton)
    in combinatorial cardinals
        {-  
  ↑     [[up], [down], 
←   →   [left], [right]]
  ↓     -}
cross : List Movements
cross = 
    let wrap = 
        ($>>) singleton << concat << tupleToList
    in wrap cardinals

forward : Piece -> Movement
forward piece = 
    case piece.color of
        White -> up
        Black -> down

backward : Piece -> Movement
backward piece = 
    case piece.color of
        White -> down
        Black -> up


stepSearch : Board -> Piece -> List Movements -> List Translation
stepSearch board piece directions =
    let go = True
        stop = False
        accum = (go, [])
        stepRange = map ((+) 1) boardside
        walk : Movements -> Int -> (Bool, List Translation) -> (Bool, List Translation)
        walk ways depth (go, paths) =
            if go
            then 
                let tracePath : Translation
                    tracePath = 
                        if length ways == 1
                        then (head ways ? always stay) depth
                        -- map dirs to depth and compose them into one move
                        else (foldl1 (>>) <| map ((|>) depth) ways) ? stay
                    target : Square
                    target = 
                    get (tracePath piece.location) board ? vacantSquare
                in 
                if isVacant target
                then (go, tracePath::paths)
                else if not <| friendlyOccupied piece.color target
                then (stop, tracePath::paths)
                else (stop, paths)             
            else (stop, paths)          
        search ways (_, paths) =
            -- override sentinel, keep going
            foldl (walk ways) (go, paths) stepRange
        (_, paths) =
            -- fold directions into list of paths
            foldl search accum directions
    in
    paths 

