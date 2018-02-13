module Data.Tool exposing (..)

import Array exposing (..)
import Matrix exposing (..)
import Mouse exposing (..)

import Data.Type exposing (..)

-- Global settings

boardside : List Int
boardside = List.range 0 7

squareSize : Int
squareSize = 54

------

(=>) = (,)

fst = Tuple.first
snd = Tuple.second

swap : (a,b) -> (b,a)
swap (a,b) = (b,a)

pos : Int -> Int -> Position
pos x_ y_ = {x=x_, y=y_}

toLocation : Position -> Location
toLocation p = loc p.y p.x -- loc col row

toPosition : (Int, Int) -> Position
toPosition (x_, y_) = {x=y_, y=x_} -- {row, col}

getPosition : Position -> Position
getPosition position = 
                                    -- minus 56px from header
    Position (position.x // squareSize) ((position.y-56) // squareSize)

toBoardPosition : Location -> Position
toBoardPosition location = 
    let p = location |> toPosition
    in Position (p.x * squareSize) (p.y * squareSize)

px : Int -> String
px value = (toString value) ++ "px"

mapMsg : (Position -> Maybe a) -> Msg -> Maybe a
mapMsg f msg =
        case msg of
            Click xy -> f xy
            Drag xy -> f xy
            Drop xy -> f xy
            _ -> Nothing

zeroLoc : Location
zeroLoc = loc 0 0

zeroPs : Position
zeroPs = { x=0, y=0 }

nullPiece : Piece
nullPiece = Piece zeroPs Black Zebra False

idleMove : Move 
idleMove = Move nullPiece zeroLoc zeroLoc Nothing

emptySquare : Square
emptySquare = Square zeroLoc Nothing False False
