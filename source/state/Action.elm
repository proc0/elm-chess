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

select : Position -> Board -> Maybe Selection
select position board = 
    let locate xy = 
            Matrix.get (toBoardLocation xy) board
        selectPiece square = 
            let selection piece = 
                Selection square.location piece
            in 
            Maybe.map selection square.piece
        selecting = 
            Maebe.join << Maybe.map selectPiece << locate
    in 
    selecting position

startMoving : Position -> Selection -> Action
startMoving ps {origin, piece} =  
    Moving <| Selection origin ({ piece | position = ps  })

updateMoving : Position -> Selection -> Action
updateMoving ps {origin, piece} = 
    Moving <| Selection origin ({ piece | position = ps })

whileMoving : Action -> (Selection -> Action) -> Action
whileMoving action change = 
    case action of
        Moving selection -> change selection
        _ -> Idle

clickMove : Board -> Player -> Move -> Position -> Location -> Action
clickMove board player lastMove pos loc =
    case player.action of
        Playing selected -> 
            let selPiece = 
                    selected.piece
                -- simulate 
                -- Moving selection
                sim = 
                    { selected
                    | piece =
                        { selPiece
                        | position = pos
                        , location = loc
                        }
                    }
            in 
            endMove board lastMove sim
        _ -> Idle

capturing : Player -> Maybe Selection -> Bool 
capturing player selection =
    selection |> Maybe.map (\s -> 
                s.piece.color /= player.color
              ) |> Maybe.withDefault False

endMove : Board -> Move -> Selection -> Action
endMove board lastMove select = 
    let destination = 
            toBoardLocation select.piece.position
        count = lastMove.number + 1
        sourcePiece = 
            select.piece |>
            (\s -> 
            { s 
            | position = toBoardPosition select.origin
            , location = select.origin
            })
        isPassant = isEnPassant board sourcePiece
        movingPiece =
            select.piece |>
            (\s -> 
            { s 
            | location = destination
            })
        target = 
            Matrix.get destination board ? emptySquare
        targetPiece =
            if isPassant
            then 
                let passantStep = 
                        backward select.piece 1 destination
                    passantSquare = 
                        Matrix.get passantStep board ? emptySquare
                in passantSquare.piece
            else 
                target.piece
    in 
    if destination /= select.origin && target.valid
    then End <| Move count select.origin destination movingPiece targetPiece isPassant
    else Playing select
