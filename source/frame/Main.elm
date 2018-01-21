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
onMouseDown =
  on "mousedown" (Json.map Grab Mouse.position)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ game, select, player } as model) =
    let 
        --logMovement = case msg of
        --    Grab ps -> log "Grab" ps
        --    Drag ps -> log "Drag" ps
        --    Drop ps -> log "Drop" ps
        --    PieceDrag pc ps -> log "PieceDrag" ps
        --    PieceDrop pc ps -> log "PieceDrop" ps

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
            _ -> Nothing

        nextGame : GameModel
        nextGame = case msg of
            Grab ps -> 
                case nextMovement of
                     Just mv -> 
                        case mv.piece of
                            Just pc -> GameModel (removePiece pc mv.start game.board) game.history
                            Nothing -> game
                     -- case for moving piece clicking squares
                     Nothing -> case select of
                                    Just sq ->
                                        case sq of
                                            Occupied p f -> GameModel ((removePiece2 f p) <| (addPiece f ps) <| game.board) game.history
                                            Vacant p -> game
                                    _ -> game
            Drop ps -> 
                case select of
                     Just sq -> 
                        case sq of
                            Occupied p f -> GameModel ((removePiece2 f p) <| (addPiece f ps) <| game.board) game.history
                            Vacant p -> game
                     Nothing -> game                     
            PieceDrop pc ps -> GameModel (addPiece pc ps game.board) game.history     
            _ -> game 
    in (Model nextGame nextSelect nextMovement, Cmd.none)

startDrag : Mouse.Position -> Maybe Moving -> Board -> Maybe Moving
startDrag ps pl board = 
        let sq = findSquare (toGamePosition ps) board
        in case sq of
                Occupied p f -> Just (Moving ps ps (Just f))
                Vacant p -> Nothing

updateDrag : Mouse.Position -> Moving -> Moving
updateDrag ps {start} = Moving start ps Nothing

updateMoving : Mouse.Position -> Piece -> Moving -> Moving
updateMoving ps pc {start} = Moving start ps (Just pc)

stopMoving : Mouse.Position -> Piece -> Moving -> Maybe Moving
stopMoving ps pc {start} = Just (Moving start ps Nothing)

findPiece : Mouse.Position -> Board -> Maybe Piece
findPiece ps board = 
    let sq = (flip findSquare board) <| toGamePosition ps
    in case sq of
            Occupied _ pc -> Just pc
            Vacant _ -> Nothing

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
            Nothing -> Vacant pos

-- board manipulations
----------------------

removePiece : Piece -> Mouse.Position -> Board -> Board
removePiece pc ps bd = updateBoard remPieceFromSquare pc ps bd

removePiece2 : Piece -> Game.Position -> Board -> Board
removePiece2 pc ps bd = updateBoard remPieceFromSquare2 pc ps bd

addPiece : Piece -> Mouse.Position -> Board -> Board
addPiece pc ps bd = updateBoard addPieceToSquare pc ps bd

addPiece2 : Piece -> Game.Position -> Board -> Board
addPiece2 pc ps bd = updateBoard addPieceToSquare2 pc ps bd

updateBoard : (Piece -> Game.Position -> Square -> Square) -> Piece -> Game.Position -> Board -> Board
updateBoard trans piece pos board =
    List.map ((\pc ps rk -> List.map (trans pc ps) rk) piece pos) board

remPieceFromSquare : Piece -> Game.Position -> Square -> Square
remPieceFromSquare pc pos sq = 
        case sq of 
            Occupied ps pec -> 
                if ps == (toGamePosition pos) && pec == pc 
                then Vacant ps 
                else Occupied ps pec
            Vacant ps -> Vacant ps

remPieceFromSquare2 : Piece -> Game.Position -> Square -> Square
remPieceFromSquare2 pc pos sq = 
        case sq of 
            Occupied ps pec -> 
                if ps == pos && pec == pc 
                then Vacant ps 
                else Occupied ps pec
            Vacant ps -> Vacant ps

addPieceToSquare : Piece -> Mouse.Position -> Square -> Square
addPieceToSquare pc mp sq =
        case sq of 
            Occupied ps pec ->
                if ps == (toGamePosition mp)
                then Occupied ps pc 
                else Occupied ps pec
            Vacant ps -> 
                if ps == (toGamePosition mp)
                then Occupied ps pc 
                else Vacant ps 

addPieceToSquare2 : Piece -> Game.Position -> Square -> Square
addPieceToSquare2 pc mp sq =
        case sq of 
            Occupied ps pec ->
                if ps == mp
                then Occupied ps pc 
                else Occupied ps pec
            Vacant ps -> 
                if ps == mp
                then Occupied ps pc 
                else Vacant ps 