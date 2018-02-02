module Model.Board exposing (..)

import Array exposing (..)
import Matrix exposing (..)

import Mouse exposing (..)

import Data.Type exposing (..)
import Data.Tool exposing (..)
import Model.FEN exposing (..)

-- board manipulations
----------------------

toggleValid : Bool -> Board -> Board
toggleValid isValid board=
        Matrix.map (\{ point, piece } -> 
            --let newPiece = Maybe.map (\p -> { p | active = isValid }) piece
            Square point piece isValid) board

liftPiece : Square -> Board -> Board
liftPiece sq bd = 
    Matrix.update (toLocation sq.point) (\s -> { s | piece = Nothing }) bd

addPiece : Mouse.Position -> Maybe Piece -> Board -> Board
addPiece ps pc bd = 
    Matrix.update (toLocation ps) (\s -> 
        if s.valid 
        then { s | piece = pc, valid = True } 
        else s) bd