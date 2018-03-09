module Model.Board exposing (..)

import Array exposing (..)
import Matrix exposing (..)
import Maybe.Extra as Maebe exposing (..)
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

undo : Board -> Piece -> Board
undo board piece = 
    return piece board 
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
    let lastLocation = last pc.path ? toLocation pc.position
        removePiece s = 
            { s 
            | piece = Nothing
            , active = True 
            }
        _ = log "removing piece" pc
    in Matrix.update lastLocation removePiece bd

add : Piece -> Board -> Board
add pc bd = 
    let lc = toBoardLocation pc.position
    in Matrix.update lc (\s -> 
        let newPiece =  
                { pc 
                | position = toBoardPosition lc
                , path = pc.path ++ [lc]
                }
            newSquare = { s | piece = Just newPiece }
            _ = log "adding piece" newPiece
        in
        if s.valid 
        then newSquare
        else s) bd

return : Piece -> Board -> Board
return piece board = 
    let putBack lc sq = 
        if sq.active 
        then { sq | piece = Just { piece | position = toBoardPosition lc } }
        else sq
    in Matrix.mapWithLocation putBack board

validate : Piece -> Board -> Board
validate piece board =
    let lc = toBoardLocation piece.position
        validMoves = pieceMoves piece board
        -- append input location as valid
        validLocations = lc::(List.map (\t -> t lc) validMoves)
        validateSquare sq = { sq | valid = True }
        validateSquares lc bd = 
            Matrix.update lc validateSquare bd
        --_ = log "valid" validLocations
    in List.foldl validateSquares board validLocations

