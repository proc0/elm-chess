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
                    Action.select position chess.board
                _ -> Nothing

        lastMove : Move
        lastMove = 
            List.head chess.history ? noMove

        -- next frame action
        action : Action
        action = 
            case event of
                -- !! click board
                Click position -> 
                    let target = 
                            toBoardLocation position
                        clickTo = 
                            -- click handler
                            clickMove chess.board player lastMove
                    in 
                    -- check selection 
                    case selection of
                        -- !! click piece
                        Just selected -> 
                            -- opponent piece click
                            if capturing player selection
                            -- capture selected piece
                            then clickTo position target
                            -- lift and drag selected piece
                            else startMoving position selected
                        -- !! click vacant square                      
                        Nothing -> 
                            clickTo position target

                -- drag piece
                Drag position -> 
                    whileMoving player.action 
                    <| updateMoving position

                -- place piece
                Drop position -> 
                    whileMoving player.action
                    <| endMove chess.board lastMove 
                            
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
                Playing selected -> 
                    -- return piece to its origin
                    revert selected.piece chess.board
                -- dragging piece
                Moving selected -> 
                    -- check last frame action
                    case player.action of
                        -- last frame was moving
                        Moving _ -> 
                            -- highlight valid moves
                            validate selected.piece chess.board
                        -- if last frame was not moving
                        otherwise -> 
                            -- start moving (lift piece)
                            -- and highlight valid moves
                            lift selected.piece chess.board 
                            |> validate selected.piece
                -- next move board            
                End move -> 
                    if move.enPassant
                    then
                        case event of
                            Click _ -> 
                                case move.capture of
                                    Just captured ->
                                        place chess.board move.piece 
                                        |> lift move.piece |> lift captured
                                    _ -> 
                                        place chess.board move.piece 
                                        |> lift move.piece
                            otherwise -> 
                                case move.capture of
                                    Just captured ->
                                        -- piece was already moving
                                        place chess.board move.piece |> lift captured
                                    _ -> 
                                        place chess.board move.piece
                    else
                        case event of
                            Click _ -> 
                                -- lift origin piece if click move
                                -- needs to place before removing? (bug?)
                                place chess.board move.piece 
                                |> lift move.piece 
                            otherwise -> 
                                -- piece was already moving
                                place chess.board move.piece                        

                Idle -> chess.board

        history : History
        history = 
            case action of
                End move -> move::chess.history
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
