module Model.Board exposing (..)

import Array exposing (..)
import Matrix exposing (..)
import Mouse exposing (..)
import Maybe.Extra as Maebe exposing (..)
import Debug exposing (..)

import Data.Type exposing (..)
import Data.Tool exposing (..)
import Model.Moves exposing (..)

--logPiece : Piece -> Board -> Board
--logPiece pc bd =
--    let _ = log "piece" pc
--    in bd

lift : Piece -> Board -> Board
lift piece board = 
    clear board 
    |> remove piece

place : Board -> Piece -> Board
place board piece = 
    board
    |> drop piece
    |> ticks >> clear

--=============================--

clearSquare :  Square -> Square
clearSquare sq =
    { sq 
    | valid = False
    , active = False 
    }

activateSquare : Square -> Square   
activateSquare sq = 
    { sq 
    | active = True 
    }

emptySquare :  Square -> Square   
emptySquare sq = 
    { sq 
    | piece = Nothing
    }

validateSquare :  Square -> Square   
validateSquare sq = 
    { sq 
    | valid = True 
    }

--=============================--

occupySquare : Piece -> Square -> Square   
occupySquare pc sq = 
    { sq 
    | piece = Just pc
    }

translatePiece : Location -> Piece -> Piece
translatePiece target piece =
    let destination = toBoardPosition target
        lastPath = last piece.path
        different = ((/=) target)
        isDifferent p = 
            Maybe.map different p ? False
    in
    { piece 
    | position = destination
    , location = target
    , path = 
        -- if the last path is 
        -- different than target
        if isDifferent lastPath
        -- add location to path
        then piece.path ++ [target]
        else piece.path
    }

withValidSquare : (Square -> Square) -> Square -> Square
withValidSquare fn square =
    if square.valid
    then fn square
    else square

withActiveSquare : (Square -> Square) -> Square -> Square
withActiveSquare fn square =
    if square.active
    then fn square
    else square

withMovingPieces : (Piece -> Piece) -> Square -> Square
withMovingPieces fn square =
    case square.piece of
        Just pc -> 
            -- if path has more than initial location
            if List.length pc.path > 1
            then -- apply piece
                { square 
                | piece = Just (fn pc) 
                }
            else square
        _ -> square

--=============================--

clear : Board -> Board
clear board =
        Matrix.map clearSquare board

ticks : Board -> Board
ticks board =
    let tickPiece _ = 
            withMovingPieces 
                (\p -> { p | tick = p.tick + 1 })
    in
    Matrix.mapWithLocation tickPiece board

remove : Piece -> Board -> Board
remove piece board = 
    let lastLocation = 
            last piece.path ? piece.location
    in 
    Matrix.update lastLocation (activateSquare << emptySquare) board

drop : Piece -> Board -> Board
drop piece board = 
    let target = piece.location
        newPiece = translatePiece target piece
    in 
    Matrix.update target (withValidSquare <| occupySquare newPiece) board

revert : Piece -> Board -> Board
revert piece board = 
    let putback target = 
            withActiveSquare (occupySquare <| translatePiece target piece)
    in 
    Matrix.mapWithLocation putback board

validate : Piece -> Board -> Board
validate piece board =
    let origin = 
            piece.location
        translations = 
            pieceMoves piece board
        movelist = 
            List.map (flip (<|) origin) translations
        validateSquares origin bd = 
            Matrix.update origin validateSquare bd
        locations = 
            if List.length translations > 0
            -- append current location
            then origin::movelist
            else []            
    in 
    List.foldl validateSquares board locations
