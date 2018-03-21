module State.Action exposing (..)

import Matrix exposing (Location, get)
import Maybe.Extra exposing ((?), join)
import Mouse exposing (Position)
import Debug exposing (log)

import Data.Type exposing (..)
import Data.Tool exposing (..)
import Model.Moves exposing (..)
import Model.Rules exposing (..)

select : Board -> Player -> Position -> Maybe Selection
select board player position = 
    let locate xy = 
            get (toBoardLocation xy) board
        selectPiece square = 
            let selection piece = 
                Selection board player square piece 
            in 
            Maybe.map selection square.piece
        selecting = 
            join << Maybe.map selectPiece << locate
    in 
    selecting position

startMoving : Position -> Selection -> Action
startMoving ps ({square, piece} as selection) =  
    Moving <| { selection | square = square, piece = ({ piece | position = ps }) }

updateMoving : Position -> Selection -> Action
updateMoving ps ({piece} as selection) = 
    Moving <| { selection | piece = ({ piece | position = ps }) }

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
    let -- calculate moving position
        destination = 
            toBoardLocation select.piece.position
        -- obtain target square
        target = 
            get destination board ? vacantSquare            
        -- update moving piece location
        movingPiece =
            select.piece |>
            (\s -> 
            { s 
            | location = destination
            })        
        -- simulate pre-move piece 
        originPiece = 
            select.piece |>
            (\s -> 
            { s 
              -- triangulate moving piece origin
            | position = toBoardPosition select.square.location
            , location = select.square.location
            })
        -- test en passant
        isPassant = 
            case originPiece.role of
                Pawn -> 
                    isEnPassant board originPiece
                _ -> False

        -- capture target
        targetPiece =
            -- check pawns for enpassant
            if isPassant
            then -- change target capture
                let captureLocation =
                        backward originPiece 1 destination
                    passantCapture = 
                        get captureLocation board ? vacantSquare
                in -- and capture pawn
                passantCapture.piece
            else -- or capture target
                target.piece
    in 
    -- if not same square, and destination is a valid move
    if destination /= select.square.location && target.valid
    then End <| Move select.square.location destination movingPiece targetPiece isPassant
    else Playing select -- keep playing
