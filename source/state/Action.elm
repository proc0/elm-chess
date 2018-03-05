module State.Action exposing (..)

import Matrix exposing (..)
import Char exposing (..)
import Maybe.Extra as Maebe exposing (..)
import Debug exposing (..)

import Mouse exposing (..)

import Data.Type exposing (..)
import Data.Tool exposing (..)
import Model.FEN exposing (..)
import Model.Rules exposing (..)

guardColor : Player -> Selection -> (Selection -> Action) -> Action
guardColor player selection doAction =                             
    if selection.piece.color == player.color
    then doAction selection
    else Idle

startMoving : Mouse.Position -> Selection -> Action
startMoving ps {location, piece} =  
    Moving <| Selection location ({ piece | position = ps })

updateMoving : Mouse.Position -> Selection -> Action
updateMoving ps {location, piece} = 
    Moving <| Selection location ({ piece | position = ps })

endMove : Board -> Mouse.Position -> Selection -> Action
endMove board ps select = 
    let destination = toBoardLocation ps
        target = Matrix.get destination board ? emptySquare
    in 
    if destination /= select.location && target.valid
    then End <| Move select.location destination select.piece target.piece
    else Undo select

whenMoving : (Selection -> Action) -> Action -> Action
whenMoving change action = 
    case action of
        Moving selection -> change selection
        _ -> Idle