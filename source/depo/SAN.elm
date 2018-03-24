module Depo.SAN exposing (..)

import Char exposing (fromCode, toCode, toLower, toUpper)
import Matrix exposing (Location, loc)
import String exposing (dropLeft, dropRight, fromChar, toInt, toLower, toList)
import Maybe exposing (map2)
import Maybe.Extra exposing ((?))
import List exposing (head)
import Debug exposing (log)

import Data.Type exposing (..)
import Depo.Lib exposing (..)
import Data.Cast exposing (..)

toSAN : Move -> String
toSAN move =
    let letter p = 
            let c = fromRole p.role
                l = if p.color == White
                    then toUpper c
                    else c
            in String.fromChar l
        file x_ = String.fromChar <| fromCode (x_ + 97)
        rank y_ = toString <| 8-y_
        noCapture mov =
            let (y,x) = mov.end
            in
            if mov.piece.role /= Pawn
            then 
                letter mov.piece 
                ++ file x 
                ++ rank y
            else 
                file x 
                ++ rank y 
        wCapture cap mov =
            let (y,x) = mov.end
            in
            if mov.piece.role == Pawn
            then
                file x
                ++ "x" 
                ++ file x 
                ++ rank y
                ++ (if move.enPassant 
                    then " e.p." 
                    else "")                
            else
                letter cap 
                ++ "x" 
                ++ file x 
                ++ rank y
    in 
    case move.capture of
        Just captured -> 
            wCapture captured move               
        _ -> 
            noCapture move


toSANLocation : String -> Maybe Location
toSANLocation san =
    let file = dropRight 1 san
        rank = dropLeft 1 san
        y_ = 
            (<?) toInt 
            << (<?) toString 
            << (<?) toCode 
            << head 
            << toList 
            <| String.toLower file
        y =
            case y_ ? Err "" of
                Ok y0 ->
                    let y1 = y0 - 97
                    in
                    if y1 >= 0 && y1 < 8
                    then Just y1
                    else Nothing
                _ -> Nothing
        x =
            case toInt rank of
                Ok x_ -> 
                    if x_ > 0 && x_ < 8
                    then Just x_
                    else Nothing
                _ -> Nothing
        _ = log "xy_" (y,x)

    in
    map2 loc y x

toRole : Char -> Role
toRole ch =
    case (Char.toLower ch) of 
        'p' -> Pawn
        'n' -> Knight
        'b' -> Bishop
        'r' -> Rook
        'q' -> Queen
        'k' -> King
        _   -> Joker

fromRole : Role -> Char
fromRole fig = 
    case fig of
        Pawn    -> 'p'
        Rook    -> 'r'
        Bishop  -> 'b'
        Knight  -> 'n'
        Queen   -> 'q'
        King    -> 'k'
        Joker   -> 'j'
