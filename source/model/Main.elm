module Model.Main exposing (fromFEN, initialBoard)

import Array exposing (..)
import Matrix exposing (..)
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

-- examples

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

-- Forsythe Edwards Notation (FEN) -> Board
fromFEN : String -> Board
fromFEN fen =
    let parts =
        String.split " " fen |> Array.fromList
    in parsePieces (Maybe.withDefault initialPieces (Array.get 0 parts))
            --(maybeContains (Array.get 1 parts) "w")
            --(maybeContains (Array.get 2 parts) "Q")
            --(maybeContains (Array.get 2 parts) "K")
            --(maybeContains (Array.get 2 parts) "q")
            --(maybeContains (Array.get 2 parts) "k")
            --(Maybe.withDefault "-" (Array.get 3 parts))
            --(Result.withDefault 0 (String.toInt (Maybe.withDefault "0" (Array.get 4 parts))))
            --(Result.withDefault 1 (String.toInt (Maybe.withDefault "0" (Array.get 5 parts))))

--maybeContains str value =
--    case str of
--        Just s ->
--            String.contains value s
--        Nothing ->
--            False

parsePieces : String -> Board
parsePieces s =
    String.split "/" s |> List.map2 mapRank boardside |> Matrix.fromList

mapRank : Int -> String -> Rank
mapRank y row =
    let pieces = expand row 
                 |> String.toList
        posons = List.map2 (,) boardside (List.repeat 8 y)
                 |> List.map toPosition
    in List.map2 toPiece posons pieces

expand : String -> String
expand s =
    Regex.replace Regex.All (Regex.regex "[12345678]") expandMatch s

expandMatch { match } =
    String.repeat (Result.withDefault 0 (String.toInt match)) " "

toPiece : Position -> Char -> Square
toPiece pos ch = 
        let sq piece = Square pos (Just piece) False
        in case ch of 
                'p' -> sq <| Piece Black Pawn False
                'n' -> sq <| Piece Black Knight False
                'b' -> sq <| Piece Black Bishop False
                'r' -> sq <| Piece Black Rook False
                'q' -> sq <| Piece Black Queen False
                'k' -> sq <| Piece Black King False
                'P' -> sq <| Piece White Pawn False
                'N' -> sq <| Piece White Knight False
                'B' -> sq <| Piece White Bishop False
                'R' -> sq <| Piece White Rook False
                'Q' -> sq <| Piece White Queen False
                'K' -> sq <| Piece White King False
                _   -> Square pos Nothing False
