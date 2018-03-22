module State.Game exposing (..)

import Char exposing (fromCode, toCode)
import Mouse exposing (Position, moves, ups)
import Keyboard exposing (downs, ups)
import Material
import Material.Layout as Layout
import Debug exposing (log)

import Data.Type exposing (..)
import Data.Tool exposing (..)
import Data.Query exposing (..)
import Data.Pure exposing (..)
import Model.Board exposing (..)
import Model.Rules exposing (..)
import State.Action exposing (..)

subscribe : Game -> Sub Event
subscribe { ui, players } = 
    let player = fst players
        layout = Layout.subs GUI ui.mdl
        persistent =
            [ layout
            , Keyboard.presses (\k ->  
                if fromCode k == '`' && ui.debug == False
                then Debug True
                else if fromCode k == '`'
                then Debug False
                else Debug ui.debug)
            ]
    in -- if player 
    case player.action of
        -- is moving
        Moving _ -> 
            Sub.batch <|
                -- track position
                [ Mouse.moves Drag
                , Mouse.ups Drop
                ]
                ++
                persistent
        _ -> Sub.batch persistent

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
        action : Action
        action = 
            case event of
                -- !! click board
                Click position -> 
                    let target = 
                            toBoardLocation position
                        clickTo = 
                            -- click handler
                            clickMove chess.board player
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
                            analyze selected.piece chess.board
                        -- if last frame was not moving
                        otherwise -> 
                            -- start moving (lift piece)
                            -- and highlight valid moves
                            grab selected.piece chess.board 
                            |> analyze selected.piece
                -- TODO: refactor using new Piece -> Board pattern
                -- next move board
                End move -> 
                    let placePiece : Move -> Board -> Board                    
                        placePiece mv bd =
                            -- if click (or drag)
                            if isClick event
                            -- lift from last loc 
                            -- and place piece
                            then drop mv.piece bd
                                    |> grab mv.piece
                            -- just place piece 
                            else drop mv.piece bd
                        eatPiece : Piece -> Board -> Board
                        eatPiece cp bd =
                            -- if click, lift captured piece
                            -- else player drops piece on it
                            if isClick event
                            then grab cp bd
                            else bd
                        -- helpers
                        movePiece = placePiece move
                        checkEnPassant fn =
                            ifEnPassant fn move
                        ifCastling fn =
                            whenCastling fn move
                    in
                    chess.board
                    |> movePiece 
                    |> ifCastling castleRook
                    |> checkEnPassant 
                        (whenCapturing eatPiece)
                    
                Idle -> 
                    chess.board

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
            , debug = 
                case event of
                    Debug debug -> debug
                    _ -> ui.debug
            }

        game mat_ =
            Game mat_ (Chess board history) players_ 
    in 
    case event of
        -- Material UI 
        GUI message -> 
            let (mat_, sub_) = 
                Material.update GUI message ui_
            in 
            game mat_ ! [sub_]
        otherwise -> 
            game ui_ ! []
