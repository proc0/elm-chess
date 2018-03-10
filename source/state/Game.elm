module State.Game exposing (..)

import Array exposing (..)
import Char exposing (..)
import Matrix exposing (..)
import Html exposing (..)
import Mouse exposing (..)
import Debug exposing (..)
import Maybe.Extra as Maebe exposing (..)
import Material
import Material.Layout as Layout

import Data.Type exposing (..)
import Data.Tool exposing (..)
import Model.FEN exposing (..)
import Model.Board exposing (..)
import State.Action as Action exposing (..)

newGame : (Game, Cmd Event)
newGame = 
    let ui = UI Material.model ""
        cmd = [Layout.sub0 Mdl]
        chess = fromFEN initialBoard
        player = idlePlayer
        players = 
            ( player White
            , player Black
            )
        game = Game ui chess players
    in game ! cmd

subs : Game -> Sub Event
subs { ui, players } = 
    let player = fst players
        layout = Layout.subs Mdl ui.mdl
    in -- if player 
    case player.action of
        -- is moving
        Moving _ -> 
            Sub.batch 
                -- track position
                [ Mouse.moves Drag
                , Mouse.ups Drop
                , layout
                ]
        _ -> layout

update : Event -> Game -> ( Game, Cmd Event )
update event { ui, chess, players } =

    let player : Player
        player = fst players

        selection : Maybe Selection 
        selection = 
            case event of
                Click position -> 
                    -- selects only if square occupied
                    Action.select position chess.board
                _ -> Nothing

        -- next frame player action
        action : Action
        action = 
            case event of
                -- click board
                Click position -> 
                    -- select board square 
                    case selection of
                        -- if target is piece
                        Just selected -> 
                            -- start dragging selected piece
                            let lift = startMoving position
                            -- guard from illegal moves
                            in guard player selected lift
                        Nothing -> 
                            let target = -- vacant square
                                Matrix.get (toBoardLocation position) chess.board
                            in
                            case target of
                                -- square click
                                Just square -> 
                                    -- if valid move
                                    if square.valid
                                    then 
                                        -- check previous action
                                        case player.action of
                                            -- piece has not moved
                                            Undo previous -> 
                                                let prev = previous.piece
                                                    -- simulate moving piece
                                                    simulated = 
                                                        { previous
                                                        | piece =
                                                            { prev
                                                            -- coerce piece state
                                                            | position = position
                                                            , location = square.location
                                                            }
                                                        }
                                                in 
                                                -- simulate end move
                                                whenMoving (Moving simulated) 
                                                <| endMove chess.board position
                                            _ -> Idle
                                    else player.action
                                _ -> Idle
                -- drag piece
                Drag position -> 
                    whenMoving player.action 
                    <| updateMoving position
                -- drop piece on board
                Drop position -> 
                    whenMoving player.action 
                    <| endMove chess.board position
                othewise -> Idle

        player_ : Player
        player_ = 
            { player 
            | action = action 
            }

        players_ = 
            case action of
                -- if end move, swap players 
                End mv -> (snd players, player_)
                -- update current player
                otherwise -> (player_, snd players)

        board : Board
        board = 
            case action of
                Moving selected -> 
                    case player.action of
                        Moving _ -> chess.board
                        otherwise -> 
                            pickup chess.board selected.piece
                End move -> 
                    let nextBoard =
                        case move.capture of
                            Just captured -> 
                                let nextMove = 
                                    drop chess.board move.piece
                                in
                                if move.enPassant
                                then nextMove |> remove captured 
                                else nextMove
                            Nothing -> drop chess.board move.piece
                    in
                    case selection of
                        Just _ -> nextBoard
                        _ -> remove move.piece nextBoard
                Undo moving -> undo moving.piece chess.board 
                otherwise -> chess.board

        history : History
        history = 
            case action of
                End mv -> mv::chess.history
                otherwise -> chess.history

        ui_ : UI
        ui_ = 
            let currentPlayer = fst players_
                currentColor = toString currentPlayer.color
            in
            { ui 
            | turn = currentColor ++ "'s turn" 
            }

        game mat_ =
            Game mat_ (Chess board history) players_ 
    in 
    case event of
        -- Material UI 
        Mdl message -> 
            let (mat_, sub_) = 
                Material.update Mdl message ui_
            in 
            game mat_ ! [sub_]
        otherwise -> 
            game ui_ ! []
