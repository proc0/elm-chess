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

select : Board -> Position -> Maybe Selection
select board position = 
    let check square = 
            let selection piece = 
                Selection square.location piece
            in 
            Maybe.map selection square.piece
        locate xy = 
            Matrix.get (toBoardLocation xy) board
        selecting = 
            Maebe.join << Maybe.map check << locate
    in 
    selecting position

guard : Player -> Selection -> (Selection -> Action) -> Action
guard player selection fn =                             
    if selection.piece.color == player.color
    then fn selection
    else Idle

startMoving : Position -> Selection -> Action
startMoving ps {location, piece} =  
    Moving <| Selection location ({ piece | position = ps })

updateMoving : Position -> Selection -> Action
updateMoving ps {location, piece} = 
    Moving <| Selection location ({ piece | position = ps })

endMove : Board -> Position -> Selection -> Action
endMove board ps select = 
    let destination = toBoardLocation ps
        step = 
            case select.piece.color of
                White -> down 
                Black -> up
        sourcePiece = 
            select.piece |>
            (\s -> 
            { s 
            | position = ps
            })
        isPassant = checkPassant board select.piece
        passante  = enPassant board select.piece
        targetLoc = 
            if isPassant 
            then select.location |> (List.head passante ? identity) 
            else destination
        target = Matrix.get targetLoc board ? emptySquare
        targetPiece = 
            if isPassant
            then (Matrix.get (step 1 targetLoc) board ? emptySquare).piece
            else target.piece
    in 
    if (destination /= select.location && target.valid) || isPassant
    then End <| Move select.location destination sourcePiece targetPiece isPassant
    else Undo select

whenMoving : (Selection -> Action) -> Action -> Action
whenMoving change action = 
    case action of
        Moving selection -> change selection
        _ -> Idle
