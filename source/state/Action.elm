module State.Action exposing (..)

import Matrix exposing (Location, get)
import Maybe.Extra exposing ((?), join)
import Mouse exposing (Position)
import Debug exposing (log)

import Data.Type exposing (..)
import Data.Tool exposing (..)
import Data.Pure exposing (..)
import Model.Moves exposing (..)
import Model.Rules exposing (..)

select : Position -> Board -> Maybe Selection
select position board = 
    let locate xy = 
            get (toBoardLocation xy) board
        selectPiece square = 
            let selection piece = 
                Selection square.location piece
            in 
            Maybe.map selection square.piece
        selecting = 
            join << Maybe.map selectPiece << locate
    in 
    selecting position

startMoving : Position -> Selection -> Action
startMoving ps ({focus, piece} as selection) =  
    Moving <| Selection focus ({ piece | position = ps })

updateMoving : Position -> Selection -> Action
updateMoving ps ({focus, piece} as selection) = 
    Moving <| Selection focus ({ piece | position = ps })

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

endMove : Board -> Selection -> Action
endMove board select = 
    let -- calculate moving position
        destination = 
            toBoardLocation select.piece.position
        -- obtain target square
        target = 
            get destination board ? vacantSquare            
        -- update moving piece location
        boarded =
            select.piece |>
            (\s -> 
            { s 
            | location = destination
            })        
        -- simulate pre-move piece 
        ghost = 
            select.piece |>
            (\s -> 
            { s 
              -- triangulate moving piece focus
            | position = toBoardPosition select.focus
            , location = select.focus
            })
        -- test en passant
        isPassant = 
            case ghost.role of
                Pawn -> 
                    isEnPassant ghost board
                _ -> False

        -- capture target
        captured =
            -- check pawns for enpassant
            if isPassant
            then -- change target capture
                let captureLocation =
                        backward ghost 1 destination
                    passantCapture = 
                        get captureLocation board ? vacantSquare
                in -- and capture pawn
                passantCapture.piece
            else -- or capture target
                target.piece
    in 
    -- if not same square, and destination is a valid move
    if destination /= select.focus && target.valid
    then End <| Move select.focus destination boarded captured isPassant
    else Playing select -- keep playing
