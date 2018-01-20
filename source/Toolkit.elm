module Toolkit exposing (..)

import Data.Game as G exposing (..)
import Settings exposing (..)

(=>) = (,)

fst = Tuple.first
snd = Tuple.second

toPosition : (Int, Int) -> G.Position
toPosition (x_, y_) = 
          { x = (x_ * squareSize)
          , y = (y_ * squareSize)
          }
--squareColor : Int -> Int -> String
--squareColor x y =
--    let black = isBlack x y
--    in if black 
--       then fst squareColors
--       else snd squareColors

px : Int -> String
px value = (toString value) ++ "px"

--isBlack : Int -> Int -> Bool
--isBlack x y = (rem (x + y) 2) == 0

--tile : G.Position -> List (Html Msg) -> Html Msg
--tile {x,y} = div []
            --[ style
            --[ 
            ----"position"        => "absolute"
            ----, "top"             => px (x * squareSize)
            ----, "left"            => px (y * squareSize)
            -- "width"           => px squareSize
            --, "height"          => px squareSize
            --, "backgroundColor" => squareColor x y
            --]
            --]