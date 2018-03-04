module Model.History exposing (..)

import Char exposing (..)

import Data.Type exposing (..)
import Data.Tool exposing (..)
import Model.FEN exposing (..)

toSAN : Move -> String
toSAN move =
        let toNotation mv = 
            let ltr = figCharMap mv.piece.role
                (y1,x1) = mv.start
                --(x2,y2) = mv.end
            in if mv.piece.role /= Pawn
               then String.fromChar (if mv.piece.color == White then toUpper ltr else ltr) ++ (String.fromChar <| fromCode (x1 + 97)) ++ (toString <| 8-y1)
               else (String.fromChar <| fromCode (x1 + 97)) ++ (toString <| 8-y1)
        in toNotation move
