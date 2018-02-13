module Model.Board exposing (..)

import Array exposing (..)
import Matrix exposing (..)

import Mouse exposing (..)

import Data.Type exposing (..)
import Data.Tool exposing (..)
import Model.FEN exposing (..)

initBoard : Board
initBoard = fromFEN initialBoard

-- board manipulations
----------------------

clear : Board -> Board
clear board = toggleValid False board

toggleValid : Bool -> Board -> Board
toggleValid isValid board =
        Matrix.map (\sq -> { sq | valid = isValid, active = False }) board

liftPiece : Piece -> Board -> Board
liftPiece pc bd = 
    Matrix.update (toLocation <| getPosition pc.position) (\s -> { s | piece = Nothing, active = True }) bd

addPiece : Move -> Board -> Board
addPiece mv bd = 
    let pc = mv.piece
    in Matrix.update mv.end (\s -> 
        if s.valid 
        then { s | piece = Just { pc | position = (toBoardPosition s.location), moved = True }, valid = False, active = False } 
        else s) bd

returnPiece : Move -> Board -> Board
returnPiece mv bd = 
    let pc = mv.piece
    in Matrix.update mv.start (\s -> 
        if s.valid 
        then { s | valid = True, active = True } 
        else s) bd