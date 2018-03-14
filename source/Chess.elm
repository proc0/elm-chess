module Chess exposing (..)

import Html exposing (..)
import Data.Type exposing (..)
import State.Game exposing (..)
import View.Main exposing (..)

main : Program Never Game Event
main = Html.program
    { init          = newGame
    , view          = render
    , update        = update
    , subscriptions = subs
    }


-- backup correct logic    
--if move.enPassant
--then
--    case event of
--        Click _ -> 
--            case move.capture of
--                Just captured ->
--                    place chess.board move.piece 
--                    |> lift move.piece |> lift captured
--                _ -> 
--                    place chess.board move.piece 
--                    |> lift move.piece
--        -- enPassant drag
--        otherwise -> 
--            case move.capture of
--                Just captured ->
--                    -- piece was already moving
--                    place chess.board move.piece |> lift captured
--                _ -> 
--                    place chess.board move.piece
--else
--    case event of
--        Click _ -> 
--            -- lift origin piece if click move
--            -- needs to place before removing? (bug?)
--            place chess.board move.piece 
--            |> lift move.piece 
--        otherwise -> 
--            -- piece was already moving
--            place chess.board move.piece
    