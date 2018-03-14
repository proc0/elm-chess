module Model.History exposing (..)

import Char exposing (..)
import Tuple exposing (..)
import Maybe.Extra as Maebe exposing (..)
import Debug exposing (..)

import Data.Type exposing (..)
import Data.Tool exposing (..)
import Model.FEN exposing (..)
import Model.SAN exposing (..)

formatHistory : History -> List String
formatHistory =
    List.map (uncurry fullMove) 
    << List.indexedMap (,)
    << List.map 
        (Tuple.mapSecond toSAN 
            << Tuple.mapFirst toSAN) 
    << toTuples 
    << List.reverse 

debugHistory : History -> String
debugHistory history =
    let lastMove = 
            List.head history ? noMove
        debugPiece : Piece -> String
        debugPiece pc =
            case pc.role of
                Ninja -> ""
                _ -> String.join "\n" <| String.split "," <| toString pc         
    in
    debugPiece lastMove.piece ++ "\ncapture:" ++ debugPiece (lastMove.capture ? nullPiece)

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
    in tuckFold <| List.foldl foldzip (Nothing, []) moves

prevPlayerColor : History -> Color
prevPlayerColor ms = 
    -- if no history, prev player was tech black
    (Maybe.map (\m -> m.piece.color) (List.head ms)) ? Black

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
