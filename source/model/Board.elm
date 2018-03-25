module Model.Board exposing (..)

import Matrix exposing (Location, flatten, get, loc, map, mapWithLocation, update)
import Maybe.Extra exposing ((?))
import List exposing (head, filterMap)
import Debug exposing (log)

import Data.Type exposing (..)
import Data.Cast exposing (..)
import Data.Pure exposing (..)
import Data.Query exposing (..)
import Depo.Lib exposing (..)
import Model.Rules exposing (..)

--logPiece : Piece -> Board -> Board
--logPiece pc bd =
--    let _ = log "piece" pc
--    in bd

grab : Piece -> Board -> Board
grab piece board = 
    clear board 
    |> remove piece

drop : Piece -> Board -> Board
drop piece board = 
    board
    |> add piece
    |> ticks >> clear

pinPiece : Piece -> Board -> Board
pinPiece pc bd =
    update pc.point (\sq ->
        { sq
        | piece =
            Just ({ pc
            | lock = True
            })
        }) bd

--checkKing : Piece -> Board -> Board
--checkKing pc bd =
--    case pc.role of
--        King ->
--            update pc.point (\sq ->
--            { sq
--            | piece =
--                Just ({ pc
--                | check = True
--                })
--            }) bd
--        _ -> bd

checkKing : Location -> Board -> Board
checkKing pt board =
    update pt (\sq ->
            case sq.piece of
                Just p ->
                    case p.role of
                        King -> 
                            { sq
                            | piece =
                                Just ({ p
                                | check = True
                                })
                            }
                        _ -> sq
                _ -> sq) board

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
    | point = target
    , drag = destination
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
            last piece.path ? piece.point
    in 
    update lastLocation (activateSquare << emptySquare) board

add : Piece -> Board -> Board
add piece board = 
    let target = piece.point
        newPiece = translatePiece target piece
    in 
    update target (withValidSquare <| occupySquare newPiece) board

jump : Piece -> Board -> Board
jump piece board = 
    let target = piece.point
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
            piece.point
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

withPinned : (Piece -> Board -> Board) -> Move -> Board -> Board
withPinned fn mv bd =
    case mv.pin of
        Just pc -> 
            fn pc bd
        _ -> bd

whenCheck : (Location -> Board -> Board) -> Move -> Board -> Board
whenCheck fn mv bd =
    let enemyKingLoc =
        (flatten bd |> filterMap (\sq ->
            case sq.piece of
                Just pc -> 
                    case pc.role of
                        King ->
                            if pc.color == opponent mv.piece.color
                            then Just sq.point
                            else Nothing
                        _ -> Nothing
                _ -> Nothing
            ) |> head) ? joker.point
    in
    if mv.check
    then fn enemyKingLoc bd
    else bd

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
            (get (fst rookMove) board ? vacantSquare).piece ? joker
    in
    remove rook board |> jump ({ rook | point = snd rookMove })

