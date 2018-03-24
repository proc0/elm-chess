module State.Action exposing (..)

import Matrix exposing (Location, get)
import Maybe.Extra exposing ((?), join)
import Mouse exposing (Position)
import Debug exposing (log)

import Data.Type exposing (..)
import Data.Cast exposing (..)
import Data.Pure exposing (..)
import Depo.Lib exposing (..)
import Depo.Moves exposing (..)
import Model.Rules exposing (..)

select : Position -> Board -> Maybe Selection
select drag board = 
    let locate xy = 
            get (toBoardLocation xy) board
        selectPiece square = 
            let selection piece = 
                Selection square.point piece
            in 
            Maybe.map selection square.piece
        selecting = 
            join << Maybe.map selectPiece << locate
    in 
    selecting drag

startMoving : Position -> Selection -> Action
startMoving ps ({focus, piece} as selection) =  
    Moving <| Selection focus ({ piece | drag = ps })

updateMoving : Position -> Selection -> Action
updateMoving ps ({focus, piece} as selection) = 
    Moving <| Selection focus ({ piece | drag = ps })

whileMoving : Action -> (Selection -> Action) -> Action
whileMoving action change = 
    case action of
        Moving selection -> change selection
        _ -> Idle

clickMove : Player -> Board -> Position -> Location -> Action
clickMove player board pos loc =
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
                        | drag = pos
                        , point = loc
                        }
                    }
            in 
            endMove board sim
        _ -> Idle

endMove : Board -> Selection -> Action
endMove board select = 
    let -- calculate moving drag
        destination = 
            toBoardLocation select.piece.drag
        -- obtain target square
        target = 
            get destination board ? vacantSquare            
        -- update moving piece point
        boarded =
            select.piece |>
            (\s -> 
            { s 
            | point = destination
            })        
        -- simulate pre-move piece 
        ghost = 
            select.piece |>
            (\s -> 
            { s 
              -- triangulate moving piece focus
            | drag = toBoardPosition select.focus
            , point = select.focus
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
