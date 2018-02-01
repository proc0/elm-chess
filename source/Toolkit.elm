module Toolkit exposing (..)

import Array exposing (..)
import Matrix exposing (..)
import Mouse exposing (..)

import Data.Chess as Game exposing (..)
import Settings exposing (..)

(=>) = (,)

fst = Tuple.first
snd = Tuple.second

pos : Int -> Int -> Game.Position
pos x_ y_ = {x=x_, y=y_}

toLocation : Game.Position -> Matrix.Location
toLocation p = loc p.y p.x

toPosition : (Int, Int) -> Game.Position
toPosition (x_, y_) = {x=x_, y=y_}

getPosition : Mouse.Position -> Game.Position
getPosition position = 
    Game.Position (position.x // squareSize) (position.y // squareSize)

px : Int -> String
px value = (toString value) ++ "px"

--isBlack : Int -> Int -> Bool
--isBlack x y = (rem (x + y) 2) == 0