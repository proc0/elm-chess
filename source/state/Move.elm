module State.Move exposing (..)

import Matrix exposing (..)
import Char exposing (..)
import Debug exposing (..)

import Mouse exposing (..)

import Data.Type exposing (..)
import Data.Tool exposing (..)
import Model.FEN exposing (..)
import Model.Rules exposing (..)

startMoving : Mouse.Position -> Selection -> Action
startMoving pos {location, piece} =  
    Moving <| Selection location ({ piece | position = pos })

updateMoving : Mouse.Position -> Selection -> Action
updateMoving pos {location, piece} = 
    Moving <| Selection location ({ piece | position = pos })

endMove : Mouse.Position -> Selection -> Action
endMove pos select = 
    let destination = toLocation <| fromMousePosition pos
    in 
    if destination /= select.location
    then End <| Move select.location destination select.piece Nothing
    else Undo select

whenMoving : (Selection -> Action) -> Action -> Action
whenMoving change action = 
    case action of
        Moving selection -> change selection
        _ -> Idle