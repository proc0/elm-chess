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
    let -- calculate moving position
        destination = 
            toBoardLocation select.piece.position
        -- obtain target square
        target = 
            Matrix.get destination board ? emptySquare            
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
            | position = toBoardPosition select.origin
            , location = select.origin
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
                        Matrix.get captureLocation board ? emptySquare
                in -- and capture pawn
                passantCapture.piece
            else -- or capture target
                target.piece
    in 
    -- if not same square, and destination is a valid move
    if destination /= select.origin && target.valid
    then End <| Move select.origin destination movingPiece targetPiece isPassant
    else Playing select -- keep playing
