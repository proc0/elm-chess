module Model.Board exposing (..)

import Matrix exposing (Location, get, loc, map, mapWithLocation, update)
import Maybe.Extra exposing ((?))
import Debug exposing (log)

import Data.Type exposing (..)
import Data.Tool exposing (..)
import Model.FEN exposing (..)
import Model.Rules exposing (..)
import Model.Query exposing (..)

-- derive from FEN notation
openingBoard : Board
openingBoard = fromFEN initialBoard

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
        map clearSquare board

ticks : Board -> Board
ticks board =
    let tickPiece _ = 
            withMovingPieces 
                (\p -> { p | tick = p.tick + 1 })
    in
    mapWithLocation tickPiece board

remove : Piece -> Board -> Board
remove piece board = 
    let lastLocation = 
            last piece.path ? piece.location
    in 
    update lastLocation (activateSquare << emptySquare) board

drop : Piece -> Board -> Board
drop piece board = 
    let target = piece.location
        newPiece = translatePiece target piece
    in 
    update target (withValidSquare <| occupySquare newPiece) board

jump : Piece -> Board -> Board
jump piece board = 
    let target = piece.location
        newPiece = translatePiece target piece
    in 
    update target (occupySquare newPiece) board

revert : Piece -> Board -> Board
revert piece board = 
    let putback target = 
            withActiveSquare (occupySquare <| translatePiece target piece)
    in 
    mapWithLocation putback board

analyze : Piece -> Board -> Board
analyze piece board =
    let origin = 
            piece.location
        translations = 
            pieceMoves piece board
        movelist = 
            List.map (flip (<|) origin) translations
        validateSquares origin bd = 
            update origin validateSquare bd
        locations = 
            if List.length translations > 0
            -- append current location
            then origin::movelist
            else []            
    in 
    List.foldl validateSquares board locations

whenCastling : (Move -> Board -> Board) -> Move -> Board -> Board
whenCastling fn mv bd =
    if isCastling mv.piece
    then fn mv bd
    else bd

whenCapturing : (Piece -> Board -> Board) -> Move -> Board -> Board
whenCapturing fn mv bd =
    case mv.capture of
        Just captured -> 
            fn captured bd
        _ -> bd
        
ifEnPassant : (Move -> Board -> Board) -> Move -> Board -> Board
ifEnPassant fn mv bd =
    if mv.enPassant 
    then fn mv bd
    else bd

castleRook : Move -> Board -> Board
castleRook move board = 
    let (y1,x1) =
            move.start
        (y2,x2) =
            move.end
        castleDiff = x2 - x1
        rookMove = 
            if isPositive castleDiff
            then ((loc y1 7), (loc y1 5))
            else ((loc y1 0), (loc y1 3))
        rook =
            (get (fst rookMove) board ? vacantSquare).piece ? nullPiece
    in
    remove rook board |> jump ({ rook | location = snd rookMove })

