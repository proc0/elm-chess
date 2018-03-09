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

-- Global tools

(=>) = (,)

fst = Tuple.first
snd = Tuple.second

swap : (a,b) -> (b,a)
swap (a,b) = (b,a)

foldl1 : (a -> a -> a) -> List a -> Maybe a
foldl1 f xs =
  let
    mf x m = Just (case m of
                     Nothing -> x
                     Just y -> f y x)
  in
    List.foldl mf Nothing xs

last : List a -> Maybe a
last = foldl1 (flip always)

-- Type castings

pos : Int -> Int -> Position
pos x_ y_ = {x=x_, y=y_}

toLocation : Position -> Location
toLocation p = loc p.y p.x -- loc col row

toPosition : (Int, Int) -> Position
toPosition (x_, y_) = {x=y_, y=x_} -- {row, col}

fromMousePosition : Position -> Position
fromMousePosition position = 
    let x = position.x // squareSize
            -- minus 56px from header
        y = (position.y-56) // squareSize
    in Position x y

toBoardPosition : Location -> Position
toBoardPosition location = 
    let p = location |> toPosition
        x = p.x * squareSize
        y = p.y * squareSize
    in Position x y

toBoardLocation : Position -> Location
toBoardLocation ps = toLocation <| fromMousePosition ps

px : Int -> String
px value = (toString value) ++ "px"

-- Nuetral type instances

zeroLoc : Location
zeroLoc = loc 0 0

zeroPs : Position
zeroPs = { x=0, y=0 }

nullPiece : Piece
nullPiece = 
    Piece zeroPs Black Zebra 0 []

emptySquare : Square
emptySquare = 
    Square zeroLoc Nothing False False

idlePlayer : Color -> Player 
idlePlayer color =
    Player color (toString color ++ " Player") Idle []

noMove : Move 
noMove =
    Move zeroLoc zeroLoc nullPiece Nothing False
