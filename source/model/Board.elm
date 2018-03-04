module Model.Board exposing (..)

import Array exposing (..)
import Matrix exposing (..)
import Debug exposing (..)

import Mouse exposing (..)

import Data.Type exposing (..)
import Data.Tool exposing (..)
import Model.FEN exposing (..)
import Model.Rules exposing (..)

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
    Matrix.update (toLocation <| fromMousePosition pc.position) (\s -> { s | piece = Nothing, active = True }) bd

addPiece : Piece -> Board -> Board
addPiece pc bd = 
    let lc = toLocation <| fromMousePosition pc.position
    in Matrix.update lc (\s -> 
        if s.valid 
        then { s | piece = Just { pc | position = (toBoardPosition lc), moved = True }, valid = False, active = False } 
        else s) bd

returnPiece : Piece -> Board -> Board
returnPiece piece board = 
    let undo lc sq = 
        if sq.active 
        then { sq | piece = Just { piece | position = toBoardPosition lc } }
        else sq
    in Matrix.mapWithLocation undo board

validate : Piece -> Board -> Board
validate piece board =
    let lc = toLocation <| fromMousePosition piece.position
        validMoves = pieceMoves piece board
        -- append input location as valid
        validLocations = lc::(List.map (\t -> t lc) validMoves)
        validateSquare sq = { sq | valid = True }
        validateSquares lc bd = 
            Matrix.update lc validateSquare bd
        --_ = log "valid" validLocations
    in List.foldl validateSquares board validLocations

