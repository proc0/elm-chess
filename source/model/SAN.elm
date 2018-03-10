module Model.SAN exposing (..)

import Char exposing (..)
import Tuple exposing (..)
import Maybe.Extra as Maebe exposing (..)
import Debug exposing (..)

import Data.Type exposing (..)
import Data.Tool exposing (..)
import Model.FEN exposing (..)

toSAN : Move -> String
toSAN move =
    let letter p = 
            let c = figCharMap p.role
                l = if p.color == White
                    then toUpper c
                    else c
            in String.fromChar l
        rank x_ = String.fromChar <| fromCode (x_ + 97)
        file y_ = toString <| 8-y_
        translate pc (x,y) =
            if pc.role /= Pawn
            then letter pc ++ rank x ++ file y
            else rank x ++ file y          
    in 
    case move.capture of
        Just captured -> 
            translate move.piece move.start 
            ++ "x" 
            ++ translate captured move.end
        _ -> translate move.piece move.end