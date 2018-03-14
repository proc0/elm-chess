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

clickMove : Board -> Player -> Position -> Location -> Action
clickMove board player pos loc =
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
            endMove board sim
        _ -> Idle

capturing : Player -> Maybe Selection -> Bool 
capturing player selection =
    selection |> Maybe.map (\s -> 
                s.piece.color /= player.color
              ) |> Maybe.withDefault False

endMove : Board -> Selection -> Action
endMove board select = 
    let destination = toBoardLocation select.piece.position
        --step = 
        --    case select.piece.color of
        --        White -> down 
        --        Black -> up
        --sourcePiece = 
        --    select.piece |>
        --    (\s -> 
        --    { s 
        --    | position = toBoardPosition select.origin
        --    , location = select.origin
        --    })
        movingPiece =
            select.piece |>
            (\s -> 
            { s 
            | location = destination
            })
        --isPassant = enPassant board select.piece
        --passante  = passant board select.piece
        --targetLoc = 
        --    if isPassant 
        --    then select.origin |> (List.head passante ? identity) 
        --    else destination
        target = Matrix.get destination board ? emptySquare
        --targetPiece =
        --    if isPassant
        --    then 
        --        let _ = log "isPassant"
        --            passantCapture = (Matrix.get (step 1 destination) board ? emptySquare).piece
        --        in passantCapture
        --    else target.piece
    in 
    if destination /= select.origin && target.valid
    then End <| Move select.origin destination movingPiece target.piece False
    else Playing select
