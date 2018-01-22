module Frame.Main exposing (..)

import Array exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Mouse exposing (..)
import Json.Decode as Json exposing (..)
import Debug exposing (..)

import Data.Main exposing (..)
import Data.Game as Game exposing (..)
import Settings exposing (..)
import Toolkit exposing (..)

onMouseDown : Attribute Msg
onMouseDown = on "mousedown" (Json.map Grab Mouse.position)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ game, select, player } as model) =
    let 
        logMovement = case msg of
            Grab ps -> log "Grab" ps
            Drag ps -> log "Drag" ps
            Drop ps -> log "Drop" ps
            PieceDrag pc ps -> log "PieceDrag" ps
            PieceDrop pc ps -> log "PieceDrop" ps

        nextMovement : Maybe Moving
        nextMovement = case msg of
            Grab ps -> startDrag ps player game.board
            Drag ps -> Maybe.map (updateDrag ps) player
            Drop _  -> Nothing
            PieceDrag pc ps -> Maybe.map (updateMoving ps pc) player
            PieceDrop pc ps -> Nothing

        nextSelect : Maybe Square
        nextSelect = case msg of
            Grab ps -> Just (findSquare (toGamePosition ps) game.board)
            PieceDrop pc ps -> select            
            PieceDrag pc ps -> select            
            _ -> Nothing

        nextGame : GameModel
        nextGame = case msg of
            Grab ps -> 
                case nextMovement of
                     Just mv -> 
                        case mv.piece of
                            Just pc -> GameModel (liftPiece pc mv.start (clearBoardHilite game.board)) game.history
                            Nothing -> game
                     -- case for moving piece clicking squares
                     Nothing -> case select of
                                    Just {pos,piece} -> 
                                        case piece of
                                            Just pc -> GameModel ((liftPiece2 pc pos) <| (addPiece pc ps) <| game.board) game.history
                                            Nothing -> game
                                    Nothing -> game
            Drop ps -> 
                case select of
                     Just {pos,piece} -> 
                        case piece of
                            Just pc -> GameModel ((liftPiece2 pc pos) <| (addPiece pc ps) <| (clearBoardHilite game.board)) game.history
                            Nothing -> game                     
                     Nothing -> game                     
            PieceDrop pc ps -> GameModel (addPiece pc ps (clearBoardHilite game.board)) game.history     
            _ -> game 
    in (Model nextGame nextSelect nextMovement, Cmd.none)

startDrag : Mouse.Position -> Maybe Moving -> Board -> Maybe Moving
startDrag ps pl board = 
        let sq = findSquare (toGamePosition ps) board
        in case sq.piece of
                Just p -> Just (Moving ps ps (Just p))
                Nothing -> Nothing

updateDrag : Mouse.Position -> Moving -> Moving
updateDrag ps {start} = Moving start ps Nothing

updateMoving : Mouse.Position -> Piece -> Moving -> Moving
updateMoving ps pc {start} = Moving start ps (Just pc)

stopMoving : Mouse.Position -> Piece -> Moving -> Maybe Moving
stopMoving ps pc {start} = Just (Moving start ps Nothing)

findPiece : Mouse.Position -> Board -> Maybe Piece
findPiece ps board = 
    let sq = (flip findSquare board) <| toGamePosition ps
    in sq.piece


findSquare : Game.Position -> Game.Board -> Square
findSquare pos board = 
    let retrieve index items = 
            case items of 
                 Just list -> list 
                    |> Array.fromList 
                    |> Array.get index
                 Nothing -> Nothing
        getSelection b = 
            retrieve pos.x <| 
            retrieve pos.y <| Just b
    in case getSelection board of
            Just match -> match
            Nothing -> Square pos Nothing False

-- board manipulations
----------------------

clearBoardHilite : Board -> Board
clearBoardHilite board = 
        List.map (\rk -> List.map (\{pos,piece,hilite} -> Square pos piece False) rk) board

liftPiece : Piece -> Mouse.Position -> Board -> Board
liftPiece pc ps bd = updateBoard remPieceFromSquare pc ps bd

liftPiece2 : Piece -> Game.Position -> Board -> Board
liftPiece2 pc ps bd = updateBoard remPieceFromSquare2 pc ps bd

addPiece : Piece -> Mouse.Position -> Board -> Board
addPiece pc ps bd = updateBoard addPieceToSquare pc ps bd

addPiece2 : Piece -> Game.Position -> Board -> Board
addPiece2 pc ps bd = updateBoard addPieceToSquare2 pc ps bd

updateBoard : (Piece -> Game.Position -> Square -> Square) -> Piece -> Game.Position -> Board -> Board
updateBoard trans piece pos board =
    List.map ((\pc ps rk -> List.map (trans pc ps) rk) piece pos) board

remPieceFromSquare : Piece -> Game.Position -> Square -> Square
remPieceFromSquare pc pos sq = 
        case sq.piece of 
            Just pec -> 
                if sq.pos == (toGamePosition pos) && pec == pc 
                then Square sq.pos Nothing True
                else sq
            Nothing -> sq

remPieceFromSquare2 : Piece -> Game.Position -> Square -> Square
remPieceFromSquare2 pc pos sq = 
        case sq.piece of 
            Just pec -> 
                if sq.pos == pos && pec == pc 
                then Square sq.pos Nothing True
                else sq
            Nothing -> sq

addPieceToSquare : Piece -> Mouse.Position -> Square -> Square
addPieceToSquare pc mp sq =
        case sq.piece of 
            Just pec ->
                if sq.pos == (toGamePosition mp)
                then Square sq.pos (Just pc) True 
                else sq
            Nothing -> 
                if sq.pos == (toGamePosition mp)
                then Square sq.pos (Just pc) True 
                else sq 

addPieceToSquare2 : Piece -> Game.Position -> Square -> Square
addPieceToSquare2 pc mp sq =
        case sq.piece of 
            Just pec ->
                if sq.pos == mp
                then Square sq.pos (Just pc) True 
                else sq
            Nothing -> 
                if sq.pos == mp
                then Square sq.pos (Just pc) False 
                else sq 