module State.Event exposing (..)

import Array exposing (..)
import Char exposing (..)
import Matrix exposing (..)
import Html exposing (..)
import Mouse exposing (..)
import Debug exposing (..)
import Maybe.Extra as Maebe exposing (..)
import Material

import Data.Type exposing (..)
import Data.Tool exposing (..)
import Model.Board exposing (..)
import State.Action exposing (..)

select : Board -> Position -> Maybe Selection
select board position = 
    let locate xy = 
            Matrix.get (toLocation <| fromMousePosition xy) board
        check square = 
            let selection piece = Selection square.location piece
            in 
            Maybe.map selection square.piece
        selecting = 
            Maebe.join << Maybe.map check << locate
    in 
    selecting position

update : Event -> Game -> ( Game, Cmd Event )
update event ({ ui, players, board, history } as game) =
    let player : Player
        player = fst players

        action : Action
        action = 
            case event of
                Click ps -> 
                    let selection = select board ps
                        guardColor sel =
                            if sel.piece.color == player.color
                            then startMoving ps sel
                            else Idle                        
                    in Maybe.map guardColor selection ? Idle
                Drag ps -> whenMoving (updateMoving ps) player.action
                Drop ps -> whenMoving (endMove ps) player.action
                _ -> Idle

        board_ : Board
        board_ = 
            case action of
                Moving selected -> 
                    case player.action of
                        Moving _ -> board
                        _ -> pickup board selected.piece
                End move -> drop board move.piece
                Undo moving -> undo board moving.piece
                _ -> board

        player_ : Player
        player_ = 
            case action of
                Undo _ -> { player | action = Idle }
                _ -> { player | action = action }

        players_ = 
            case action of
                End mv -> (snd players, player_)
                _ -> (player_, snd players)

        hist_ : History
        hist_ = 
            case action of
                End mv -> mv::history
                _ -> history

        ui_ : UI
        ui_ = 
            { ui 
            | turn = toString (fst players_).color ++ "'s turn" 
            }

        game_ mat_ =
            Game mat_ players_ board_ hist_
    in 
    case event of
        Mdl message -> 
            -- update Material UI 
            let (mat_, sub_) = 
                Material.update Mdl message ui_
            in game_ mat_ ! [sub_]
        _ -> game_ ui_ ! []
