module Depo.Moves exposing (..)

import Matrix exposing (Location, loc, get)
import List exposing (foldl, any, map, concat, concatMap, filterMap, length, head, reverse, singleton)
import Maybe.Extra exposing ((?))
import Debug exposing (log)

import Data.Cast exposing (..)
import Data.Type exposing (..)
import Data.Query exposing (..)
import Data.Pure exposing (..)
import Depo.Lib exposing (..)
import Config.Settings exposing (..)

-- board translations
-- ==================
idle : Translation
idle l = l

up : Movement
up n (y,x) = 
    loc (y - n) x

down : Movement
down n (y,x) = 
    loc (y + n) x

left : Movement
left n (y,x) = 
    loc y (x - n)

right : Movement
right n (y,x) = 
    loc y (x + n)

--forward : Location -> Location
--forward (y,x) =
--backward : Location -> Location

forwardMove : Piece -> Movement
forwardMove piece = 
    case piece.color of
        White -> up
        Black -> down

backwardMove : Piece -> Movement
backwardMove piece = 
    case piece.color of
        White -> down
        Black -> up

cardinals : (Movements, Movements)
cardinals = 
    ( [up, down]
    , [left, right]
    )

        {-  
  ↑     [[up], [down], 
← · →   [left], [right]]
  ↓     -}
straights : List Movements
straights = 
    let wrap = 
        ($>>) singleton << concat << tupleToList
    in 
    wrap cardinals

        {-
↖   ↗   [... [up, left],    
  ·     [down, right] ... ]
↙   ↘   -}
diagonals : List Movements
diagonals = 
    let cartesian = 
        (uncurry <| liftAp (++)) 
            << mapBoth (($>>) singleton)
    in 
    cartesian cardinals

        {-
↖ ↑ ↗   all possible movements,    
← · →   asterisk shaped arrows
↙ ↓ ↘   -}
asterisk : List Movements
asterisk = diagonals ++ straights

{-       ♝
  ↑    ↙  ↑ ↘ 
  ↑ ↖↙    ↑   ↘  
♙ ♖ ♙↖    ↑   ↗ ?
  ↓     ↖ ↑ ↗
← ↓ ← ← ← ♕  → →
linear direction piece - move logic -}
linearMove : Piece -> Board -> List Movements -> Translations
linearMove piece board directions =
    let searchWhen = 
            isVacant
        stopWhen = 
            not << friendlyOccupied piece.color
        search =
            stepSearch piece board
        consAll = 
            ((::), (::))
    in 
    -- search in some linear direction,
    -- and stop when opponent piece is found
    search (searchWhen, stopWhen) consAll directions

{-  ?           ♔      step square by square in some direction,
      ↖   ↑   ↗        test square to keep going, or pass square
        ↖ ↑ ↗          to final transform function
←  ♘  ← ← · → → → → ?
        ↙ ↓ ↘       
      ↙   ↓   ↘
-}
stepSearch : 
    Piece 
    -> Board 
    -> ((Square -> Bool), (Square -> Bool)) 
    -> ((Translation -> Translations -> Translations), (Translation -> Translations -> Translations)) 
    -> List Movements 
    -> Translations
stepSearch piece board semaphores transforms directions =
    let go = True
        stop = False
        memo = (go, [])
        step = map ((+) 1) boardside
        green = fst semaphores
        red = snd semaphores
        relay = fst transforms
        finish = snd transforms

        walk : Movements 
            -> Int 
            -> (Bool, Translations) 
            -> (Bool, Translations)
        walk ways depth (go, paths) =
            if go
            then 
                let tracePath : Translation
                    tracePath = 
                        if length ways == 1
                        then (head ways ? always idle) depth
                        -- map dirs to depth and compose them into one move
                        else (foldl1 (>>) <| map ((|>) depth) ways) ? idle
                    target : Square
                    target = 
                    get (tracePath piece.point) board ? vacantSquare
                in 
                if green target
                then (go, relay tracePath paths)
                else if red target
                then (stop, finish tracePath paths)
                else (stop, paths)            
            else (stop, paths)          
        search ways (_, paths) =
            -- override sentinel, keep going
            foldl (walk ways) (go, paths) step
        (_, paths) =
            -- fold directions into list of paths
            foldl search memo directions
    in
    paths 

