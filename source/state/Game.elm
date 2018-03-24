module State.Game exposing (..)

import Char exposing (fromCode, toCode)
import Mouse exposing (Position, moves, ups)
import Keyboard exposing (KeyCode, downs, ups)
import Material
import Material.Layout as Layout
import Debug exposing (log)

import Data.Type exposing (..)
import Data.Cast exposing (..)
import Data.Query exposing (..)
import Data.Pure exposing (..)
import Depo.Lib exposing (..)
import Model.Board exposing (..)
import Model.Rules exposing (..)
import State.Action exposing (..)

toggleDebug : Bool -> KeyCode -> Event
toggleDebug debugging key = 
    let tilde =
            fromCode key == '`'
    in
    Debug <| (tilde && not debugging) || (not debugging)

subscribe : Game -> Sub Event
subscribe { ui, players } = 
    let default =
            [ Keyboard.presses 
                <| toggleDebug ui.debug
            , Layout.subs GUI ui.mdl
            ]
    in -- if current player 
    case (fst players).action of
        -- is moving
        Moving _ -> 
            Sub.batch <|
                -- trackey position
                [ Mouse.moves Drag
                , Mouse.ups Drop
                ]
                ++
                default
        _ -> Sub.batch default

update : Event -> Game -> ( Game, Cmd Event )
update event { ui, chess, players } =

    let player : Player
        player = fst players

        selection : Maybe Selection 
        selection = 
            case event of
                Click position -> 
                    select position chess.board
                _ -> Nothing

        -- next frame action
        action_ = 
            case event of
                -- !! click board
                Click position -> 
                    let target = 
                            toBoardLocation position
                        clickTo = 
                            -- click handler
                            clickMove player chess.board
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
                        <| endMove chess.board 
                            
                other -> Idle

        player_ = 
            { player 
            | action = action_
            , pieces =
                case action_ of
                End move -> 
                    player.pieces
                    ++
                    [move.piece]
                other -> 
                    player.pieces
            }

        players_ = 
            case action_ of
                -- if end move, swap players 
                End mv -> (snd players, player_)
                -- update current player
                other -> (player_, snd players)

        board_ = 
            case action_ of
                -- neutral
                Playing selected -> 
                    revert selected.piece chess.board
                -- dragging piece
                Moving selected -> 
                    let piece = selected.piece
                        preform fn = fn piece chess.board
                    in
                    -- check last frame
                    case player.action of
                        -- highlight possible moves
                        Moving _ -> preform analyze
                        -- player started move
                        other -> 
                            -- drag piece and highlight
                            preform grab |> analyze piece
                -- next board
                End move -> 
                    let eat : Piece -> Board -> Board
                        eat captured =
                            if isClick event
                            -- cleanup captured
                            then grab captured
                            else identity
                        place : Piece -> Board -> Board                    
                        place piece =
                            drop piece
                            -- cleanup captured
                            >> (if isClick event
                                then eat piece
                                else identity)                          
                    in
                    place move.piece chess.board
                    |> whenCastling castleRook move
                    |> (if move.enPassant 
                        then whenCapturing eat move 
                        else identity)
                    
                Idle -> chess.board

        history_ = 
            case action_ of
                End move -> move::chess.history
                other -> chess.history

        ui_ = 
            let currentPlayer = fst players_
                currentColor = toString currentPlayer.color
            in
            { ui 
            | turn = currentColor ++ "'s turn"
            , debug = 
                case event of
                    Debug debug -> debug
                    _ -> ui.debug
            }

        game mat_ =
            Game mat_ (Chess board_ history_) players_ 
    in 
    case event of
        -- Material UI 
        GUI message -> 
            let (mat_, sub_) = 
                Material.update GUI message ui_
            in 
            game mat_ ! [sub_]
        other -> 
            game ui_ ! []
