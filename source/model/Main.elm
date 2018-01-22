module Model.Main exposing (fromFEN, initialBoard)

-- Forsythe Edwards Notation (FEN) -> GameModel

import Array exposing (..)
import Debug exposing (..)

import Data.Main exposing (..)
import Data.Game exposing (..)
import Toolkit exposing (..)
import Settings exposing (..)
import String
import Regex

initialPieces =
    "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"

initialBoard =
    initialPieces ++ " w KQkq - 0 1"

-- Some useful boards for debugging

--testingEngine =
--    "K6k/3n2pp/8/1P6/B7/8/6PP/8 w - - 0 1"

--whiteInCheck =
--    "rnb1kbnr/pppppppp/4q3/8/8/8/PPP2PPP/RNBQKBNR w KQkq - 0 1"

--blackCheckMate =
--    "1R6/8/kQ6/8/8/8/6K1/8 w - - 0 1"

--enPassantBoard =
--    "rnbqkbnr/1ppppppp/8/pP6/8/8/P1PPPPPP/RNBQKBNR w KQkq a6 0 1"

--castlingAvailable =
--    "rnbqkbnr/1ppppppp/8/pP6/8/8/P1PPPPPP/R3K2R w KQkq - 0 1"

fromFEN : String -> GameModel
fromFEN fen =
    let parts =
        String.split " " fen |> Array.fromList
    in GameModel (parsePieces (Maybe.withDefault initialPieces (Array.get 0 parts))) []
            --(maybeContains (Array.get 1 parts) "w")
            --(maybeContains (Array.get 2 parts) "Q")
            --(maybeContains (Array.get 2 parts) "K")
            --(maybeContains (Array.get 2 parts) "q")
            --(maybeContains (Array.get 2 parts) "k")
            --(Maybe.withDefault "-" (Array.get 3 parts))
            --(Result.withDefault 0 (String.toInt (Maybe.withDefault "0" (Array.get 4 parts))))
            --(Result.withDefault 1 (String.toInt (Maybe.withDefault "0" (Array.get 5 parts))))

parsePieces : String -> Board
parsePieces s =
    String.split "/" s |> List.map2 mapRank boardside

mapRank : Int -> String -> Rank
mapRank y row =
    let pieces = expand row 
                 |> String.toList
        posons = List.map2 (,) boardside (List.repeat 8 y)
                 |> List.map toPosition
    in List.map2 toPiece posons pieces

--maybeContains str value =
--    case str of
--        Just s ->
--            String.contains value s
--        Nothing ->
--            False

expand : String -> String
expand s =
    Regex.replace Regex.All (Regex.regex "[12345678]") expandMatch s

expandMatch { match } =
    String.repeat (Result.withDefault 0 (String.toInt match)) " "

toPiece : Position -> Char -> Square
toPiece pos ch = 
        let sq piece = Square pos (Just piece) False
        in case ch of 
                'p' -> sq <| Black Pawn
                'n' -> sq <| Black Knight
                'b' -> sq <| Black Bishop
                'r' -> sq <| Black Rook
                'q' -> sq <| Black Queen
                'k' -> sq <| Black King
                'P' -> sq <| White Pawn
                'N' -> sq <| White Knight
                'B' -> sq <| White Bishop
                'R' -> sq <| White Rook
                'Q' -> sq <| White Queen
                'K' -> sq <| White King
                _   -> Square pos Nothing False
