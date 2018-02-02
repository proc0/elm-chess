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

pos : Int -> Int -> Point
pos x_ y_ = {x=x_, y=y_}

toLocation : Point -> Matrix.Location
toLocation p = loc p.y p.x

toPosition : (Int, Int) -> Point
toPosition (x_, y_) = {x=x_, y=y_}

getPosition : Mouse.Position -> Point
getPosition position = 
    Point (position.x // squareSize) (position.y // squareSize)

px : Int -> String
px value = (toString value) ++ "px"

--isBlack : Int -> Int -> Bool
--isBlack x y = (rem (x + y) 2) == 0

mapMsg : (Mouse.Position -> a) -> Msg -> a
mapMsg f msg =
        case msg of
            Click xy -> f xy
            Drag xy -> f xy
            Drop xy -> f xy
