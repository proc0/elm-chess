module Model.Board exposing (..)

import Array exposing (..)
import Matrix exposing (..)

import Mouse exposing (..)

import Data.Type exposing (..)
import Data.Tool exposing (..)
import Model.FEN exposing (..)

-- board manipulations
----------------------

clear : Board -> Board
clear board = toggleValid False board

toggleValid : Bool -> Board -> Board
toggleValid isValid board=
        Matrix.map (\{ point, piece, active } -> 
            --let newPiece = Maybe.map (\p -> { p | active = isValid }) piece
            Square point piece isValid False) board

liftPiece : Move -> Board -> Board
liftPiece mv bd = 
    Matrix.update (toLocation mv.start) (\s -> { s | piece = Nothing, active = True }) bd

addPiece : Move -> Board -> Board
addPiece mv bd = 
    let pc = mv.piece
    in Matrix.update (toLocation mv.end) (\s -> 
        if s.valid 
        then { s | piece = Just { pc | moved = True }, valid = True, active = False } 
        else s) bd

returnPiece : Move -> Board -> Board
returnPiece mv bd = 
    let pc = mv.piece
    in Matrix.update (toLocation mv.start) (\s -> 
        if s.valid 
        then { s | valid = True, active = True } 
        else s) bd