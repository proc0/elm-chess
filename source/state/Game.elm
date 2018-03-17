module State.Game exposing (..)

import Debug exposing (log)
import Mouse exposing (Position, moves, ups)
import Material
import Material.Layout as Layout
--import Maybe.Extra exposing ((?))

import Data.Type exposing (..)
import Data.Tool exposing (..)
import Model.Board exposing (..)
import State.Action exposing (..)

newGame : (Game, Cmd Event)
newGame = 
    let -- UI events and subs
        ui = UI Material.model ""
        cmd = [Layout.sub0 Mdl]
        -- prep game args
        chess = Chess openingBoard []
        players = 
            ( idlePlayer White
            , idlePlayer Black
            )
        game = Game ui chess players
    in game ! cmd

subscribe : Game -> Sub Event
subscribe { ui, players } = 
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
                            lift selected.piece chess.board 
                            |> analyze selected.piece
                -- next move board
                End move -> 
                    let isClick : Bool
                        isClick = 
                            case event of
                                Click _ -> True
                                _ -> False
                        placePiece : Move -> Board -> Board                    
                        placePiece mv bd =
                            -- if click (or drag)
                            if isClick
                            -- lift from last loc 
                            -- and place piece
                            then place bd mv.piece 
                                    |> lift mv.piece
                            -- just place piece 
                            else place bd mv.piece
                        eatPiece : Piece -> Board -> Board
                        eatPiece cp bd =
                            -- if click, lift captured piece
                            -- else player drops piece on it
                            if isClick
                            then lift cp bd
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
