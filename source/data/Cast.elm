module Data.Cast exposing (..)

import Matrix exposing (Location, loc)
import Mouse exposing (Position)
import List exposing (map)
import Tuple exposing (first, second)

import Data.Type exposing (..)
import Config.Settings exposing (..)

-- Type castings

tupleToList : (a,a) -> List a
tupleToList t = [first t, second t]

pos : Int -> Int -> Position
pos x_ y_ = {x=x_, y=y_}

toLocation : Position -> Location
toLocation p = loc p.y p.x -- loc col row

toLocations : List (Int, Int) -> List Location
toLocations = map (uncurry loc)

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
