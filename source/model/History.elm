module Model.History exposing (..)

import Char exposing (..)
import Debug exposing (..)

import Data.Type exposing (..)
import Data.Tool exposing (..)
import Model.FEN exposing (..)

toSAN : Move -> String
toSAN move =
    let letter = 
            let c = figCharMap move.piece.role
                l = if move.piece.color == White
                    then toUpper c
                    else c
            in String.fromChar l
        (y1,x1) = move.start
        (x2,y2) = move.end
        rank = String.fromChar <| fromCode (x2 + 97)
        file = toString <| 8-y2
    in 
    if move.piece.role /= Pawn
    then letter ++ rank ++ file
    else rank ++ file

fullMove : Int -> (String, String) -> String
fullMove i (w,b) =
    toString i ++ ". " ++ w ++ " " ++ b