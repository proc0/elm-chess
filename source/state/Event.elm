module State.Event exposing (..)

import Array exposing (..)
import Char exposing (..)
import Matrix exposing (..)
import Html exposing (..)
import Mouse exposing (..)
import Debug exposing (..)
import Maybe.Extra as Maebe exposing (..)
import Material.Layout as Layout
import Material

import Data.Type exposing (..)
import Data.Tool exposing (..)
import Model.FEN exposing (..)
import Model.Board exposing (..)
import State.Move exposing (..)

subscriptions : Game -> Sub Msg
subscriptions { ui, players } = 
    let player = fst players 
        trackUI = Layout.subs Mdl ui.mdl                  
        trackDrag = 
            Sub.batch 
                [ Mouse.moves Drag
                , Mouse.ups Drop
                , trackUI
                ]
    in 
    case player.moving of
        Just _  -> trackDrag
        Nothing -> trackUI

update : Msg -> Game -> ( Game, Cmd Msg )
update msg 
    ({ ui
     , players
     , board
     , history
     } 
     as game) =

    let -- get selected square
        selected : Maybe Square
        selected = 
            let target mouse = 
                Matrix.get (toLocation <| getPosition mouse) board
            in 
            msg |> mapMsg (\xy -> 
                    Maebe.join <| Maybe.map (\t -> 
                        t.piece |> Maybe.map (always t)) <| target xy)

        currentPlayer : Player
        currentPlayer = fst players

        nextPlayer : Player
        nextPlayer = 
            let makeNext : Maybe Square -> Maybe Piece -> Player
                makeNext sq pc = 
                { currentPlayer
                | select = sq
                , moving = pc
                }
            in 
            case msg of
                Click xy ->
                    case selected of
                        Just sel -> makeNext selected (updatePiecePos xy sel.piece)
                        _ -> makeNext Nothing Nothing
                Drag xy -> { currentPlayer | moving = updatePiecePos xy currentPlayer.moving }
                Drop xy -> makeNext selected Nothing
                _ -> currentPlayer      

        nextMove : Maybe Move
        nextMove =
            case msg of
                Drop xy -> 
                    case currentPlayer.select of
                        Just sel -> 
                            case selected of
                                Just sel2 -> 
                                    if sel.location /= sel2.location
                                    then
                                        case sel.piece of
                                            Just pc ->
                                                case sel2.piece of
                                                    Just pc2 -> Just <| Move pc sel.location sel2.location sel2.piece
                                                    Nothing -> Just <| Move pc sel.location sel2.location Nothing
                                            Nothing -> Nothing
                                    else
                                        Nothing
                                _ -> 
                                    case sel.piece of
                                        Just pc -> Just <| Move pc sel.location (toLocation <| getPosition xy) Nothing
                                        Nothing -> Nothing
                        _ -> Nothing                
                _ -> Nothing

        nextPlayers = 
            case nextMove of
                Just _ -> (snd players, nextPlayer)
                _ -> (nextPlayer, snd players)

        -- calculate next board 
        -- based on player move
        nextBoard : Board
        nextBoard = 
            let clear = toggleValid False

                default : Board
                default = clear board

                start : Piece -> Board
                start piece = default 
                        |> validate (Move piece (toLocation <| getPosition piece.position) (toLocation <| getPosition piece.position) Nothing)
                        |> liftPiece piece

                endMove : Move -> Board
                endMove move = default
                        --|> validate move
                        |> addPiece move
                        --|> clear

                undoMove : Move -> Board
                undoMove move = returnPiece move board |> clear

            in 
            case nextMove of
                Just mv -> endMove mv
                _ -> 
                    case currentPlayer.moving of
                        Just curMvg -> board
                        _ ->
                            case nextPlayer.moving of
                                Just pc -> start pc
                                _ -> board

        -- update history based on next move
        nextHistory : History
        nextHistory = 
            case nextMove of
                Just mv -> mv::history
                _ -> history

        nextUI : UI
        nextUI = { ui | turn = toString currentPlayer.color ++ "'s turn" }

        _ = log "log" nextMove
    in case msg of
        Mdl message -> 
            let (nextMaterial, subMsg) = Material.update Mdl message nextUI
            in Game nextMaterial nextPlayers board history ! [subMsg]                         
        _ -> 
            Game nextUI nextPlayers nextBoard nextHistory ! []