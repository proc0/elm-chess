module Model.History exposing (..)

import Char exposing (isUpper, toLower)
import Tuple exposing (mapFirst, mapSecond)
import List exposing (foldl, head, indexedMap, map, reverse)
import Maybe.Extra exposing ((?))
import Debug exposing (log)

import Data.Type exposing (..)
import Data.Tool exposing (..)
import Data.Pure exposing (..)
import Model.FEN exposing (..)
import Model.SAN exposing (..)

formatHistory : History -> List String
formatHistory =
    map (uncurry fullMove) 
    << indexedMap (,)
    << map 
        (mapSecond toSAN 
            << mapFirst toSAN) 
    << toTuples 
    << reverse 

debugHistory : History -> String
debugHistory history =
    let lastMove = 
            head history ? noMove
        formatPiece p =
            String.join " " [(toString p.color), (toString p.role), (toString p.location)]
        debugPiece : Piece -> String
        debugPiece pc =
            case pc.role of
                Joker -> ""
                _ -> formatPiece pc         
    in
    debugPiece lastMove.piece ++ "\ncapture:\n" ++ debugPiece (lastMove.capture ? nullPiece)

fullMove : Int -> (String, String) -> String
fullMove i (w,b) =
    if w /= b
    then toString (i+1) ++ ". " ++ w ++ " " ++ b
    else toString (i+1) ++ ". " ++ w

toTuples : History -> List (Move, Move)
toTuples moves =
    let foldzip : Move -> (Maybe Move, List(Move, Move)) -> (Maybe Move, List(Move, Move))
        foldzip mv (mx, ms) =
            case mx of
                Just mv_ -> (Nothing, ms ++ [(mv_, mv)])
                _ -> (Just mv, ms)
        tuckFold (mx, ms) =
            case mx of
                Just mv_ -> ms ++ [(mv_, mv_)]
                _ -> ms
    in tuckFold <| foldl foldzip (Nothing, []) moves

prevPlayerColor : History -> Color
prevPlayerColor ms = 
    -- if no history, prev player was tech black
    (Maybe.map (\m -> m.piece.color) (head ms)) ? Black

getPlayerByColor : Color -> History -> Players -> Player
getPlayerByColor color history (curPlayer, prevPlayer) = 
    let otherColor = 
            case color of
                White -> Black
                Black -> White
    in
    if prevPlayerColor history == otherColor && curPlayer.color /= otherColor
    then curPlayer
    else prevPlayer

getWhite : History -> Players -> Player
getWhite = getPlayerByColor White 

getBlack : History -> Players -> Player
getBlack = getPlayerByColor Black 
