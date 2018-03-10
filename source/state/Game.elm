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

        -- next frame player action
        action : Action
        action = 
            case event of
                -- clicks board
                Click position -> 
                    -- select board square 
                    (Action.select chess.board position 
                        |> Maybe.map (\selection -> 
                            let lift = startMoving position
                            -- guard from illegal moves
                            -- and start dragging selected piece
                            in guard player selection lift
                        )) ? Idle
                -- drags piece
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

        -- swap players 
        -- if end of turn
        players_ = 
            case action of
                End mv -> (snd players, player_)
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
                    case move.capture of
                        Just captured -> 
                            let nextMove = 
                                drop chess.board move.piece
                            in
                            if move.enPassant
                            then nextMove |> remove captured 
                            else nextMove
                        Nothing -> drop chess.board move.piece
                Undo moving -> undo moving.piece chess.board 
                otherwise -> chess.board

        history : History
        history = 
            case action of
                End mv -> mv::chess.history
                otherwise -> chess.history

        ui_ : UI
        ui_ = 
            { ui 
            | turn = toString (fst players_).color ++ "'s turn" 
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
