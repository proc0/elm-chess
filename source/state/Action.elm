module State.Action exposing (..)

import Matrix exposing (Location, get, mapWithLocation)
import Maybe.Extra exposing ((?), join, isJust)
import List exposing (any, length, head, map)
import Mouse exposing (Position)
import Debug exposing (log)

import Data.Type exposing (..)
import Data.Cast exposing (..)
import Data.Pure exposing (..)
import Data.Query exposing (..)
import Depo.Lib exposing (..)
import Depo.Moves exposing (..)
import Model.Rules exposing (..)

select : Position -> Board -> Maybe Selection
select drag board = 
    let locate xy = 
            get (toBoardLocation xy) board
        selectPiece square = 
            let selectP piece = 
                if not piece.lock
                then Just <| Selection square.point piece
                else Nothing
            in 
            join <| square.piece ?> selectP
        selecting = 
            join << (<?) selectPiece << locate
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
        piece =
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
        isCheck =
            map (\tr -> 
                let sq = 
                    get (tr destination) board ? vacantSquare
                in
                case sq.piece of
                    Just pc -> 
                        case pc.role of
                            King -> pc.color /= piece.color
                            _ -> False
                    _ -> False) (pieceMoves piece board) |> any identity
        pinTarget =
            let targetSquare = 
                    head <| withPinner piece board findNextOpponent
                target = 
                    targetSquare ?> (flip get board << (|>) piece.point) |> join
            in
            (target ? vacantSquare).piece
        -- capture target
        captured =
            -- check pawns for enpassant
            if isPassant
            then -- change target capture
                let captureLocation =
                        backwardMove piece 1 destination
                    passantCapture = 
                        get captureLocation board ? vacantSquare
                in -- and capture pawn
                passantCapture.piece
            else -- or capture target
                target.piece
        pinPiece =
           if not <| isCheck
           then pinTarget ?> (\p -> if not <| isKing p then Just p else Nothing) |> join
           else Nothing

        --isCheck =
        --    isJust kingThreat && (isJust <| (pinTarget ?> isKing))
    in 
    -- if not same square, and destination is a valid move
    if destination /= select.focus && target.valid && not piece.lock
    then End <| Move select.focus destination piece captured pinPiece isCheck isPassant
    else Playing select -- keep playing
