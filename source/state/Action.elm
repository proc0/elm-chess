module State.Action exposing (..)

import Matrix exposing (..)
import Char exposing (..)
import Maybe.Extra as Maebe exposing (..)
import Mouse exposing (..)
import Debug exposing (..)

import Data.Type exposing (..)
import Data.Tool exposing (..)
import Model.FEN exposing (..)
import Model.Moves exposing (..)

select : Board -> Position -> Maybe Selection
select board position = 
    let focus square = 
            let selection piece = 
                Selection square.location piece
            in 
            Maybe.map selection square.piece
        locate xy = 
            Matrix.get (toBoardLocation xy) board
        selecting = 
            Maebe.join << Maybe.map focus << locate
    in 
    selecting position

guard : Player -> Selection -> (Selection -> Action) -> Action
guard player selection fn =                             
    if selection.piece.color == player.color
    then fn selection
    else Idle

startMoving : Position -> Selection -> Action
startMoving ps {origin, piece} =  
    Moving <| Selection origin ({ piece | position = ps, location = origin  })

updateMoving : Position -> Selection -> Action
updateMoving ps {origin, piece} = 
    Moving <| Selection origin ({ piece | position = ps })

whenMoving : Action -> (Selection -> Action) -> Action
whenMoving action change = 
    case action of
        Moving selection -> change selection
        _ -> Idle

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
            , location = toBoardLocation ps
            })
        isPassant = enPassant board select.piece
        passante  = passant board select.piece
        targetLoc = 
            if isPassant 
            then select.origin |> (List.head passante ? identity) 
            else destination
        target = Matrix.get targetLoc board ? emptySquare
        targetPiece = 
            if isPassant
            then (Matrix.get (step 1 targetLoc) board ? emptySquare).piece
            else target.piece
    in 
    if (destination /= select.origin && target.valid) || isPassant
    then End <| Move select.origin destination sourcePiece targetPiece isPassant
    else Undo select
