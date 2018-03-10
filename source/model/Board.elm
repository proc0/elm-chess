module Model.Board exposing (..)

import Array exposing (..)
import Matrix exposing (..)
import Mouse exposing (..)
import Maybe.Extra as Maebe exposing (..)
import Debug exposing (..)

import Data.Type exposing (..)
import Data.Tool exposing (..)
import Model.Moves exposing (..)

-- board manipulations
----------------------
pickup : Board -> Piece -> Board
pickup board piece = 
    clear board 
    |> validate piece
    |> remove piece

drop : Board -> Piece -> Board
drop board piece = 
    board
    |> add piece
    |> ellapse
    |> clear

--logPiece : Piece -> Board -> Board
--logPiece pc bd =
--    let _ = log "piece" pc
--    in bd

clear : Board -> Board
clear board =
        Matrix.map (\sq -> { sq | valid = False, active = False }) board

ellapse : Board -> Board
ellapse board =
    let calcEllaps p =
            -- if piece has more 
            -- than initial location
            if List.length p.path > 1
            then p.ellapsed + 1 
            else p.ellapsed         
    in 
    board 
    |> Matrix.map (\sq -> 
        case sq.piece of
            Just pc -> 
                { sq 
                | piece = Just ({ pc | ellapsed = calcEllaps pc }) 
                }
            _ -> sq)

remove : Piece -> Board -> Board
remove pc bd = 
    let lastLocation = last pc.path ? pc.location
        removePiece s = 
            { s 
            | piece = Nothing
            , active = True 
            }
    in Matrix.update lastLocation removePiece bd

add : Piece -> Board -> Board
add pc bd = 
    let lc = pc.location
    in Matrix.update pc.location (\s -> 
        let newPiece =  
                { pc 
                | position = toBoardPosition lc
                , location = lc
                , path = pc.path ++ [lc]
                }
            newSquare = { s | piece = Just newPiece }
        in
        if s.valid 
        then newSquare
        else s) bd

undo : Piece -> Board -> Board
undo piece board = 
    let putBack lc sq = 
        if sq.active 
        then { sq | piece = Just { piece | position = toBoardPosition lc, location = lc } }
        else sq
    in Matrix.mapWithLocation putBack board

validate : Piece -> Board -> Board
validate piece board =
    let plc = piece.location
        translations = pieceMoves piece board
        -- append input location as valid
        moveList = List.map (flip (<|) plc) translations
        validateSquare sq = { sq | valid = True }
        validateSquares plc bd = 
            Matrix.update plc validateSquare bd
        locations = 
            if List.length translations > 1
            then plc::moveList
            else []
        validBoard = 
            List.foldl validateSquares board locations
        _ = log "translations" locations
    in 
    validBoard
