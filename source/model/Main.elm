module Model.Main exposing (fromFEN, initialBoard)

-- Forsythe Edwards Notation (FEN) -> GameModel

import Array exposing (..)
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
    in GameModel (parsePieces (Maybe.withDefault initialPieces (Array.get 0 parts))) (Vacant {x=0,y=0}) []
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
mapRank x row =
    let pieces = expand row 
                 |> String.toList
        posons = List.map2 (,) (List.repeat 8 x) boardside
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
        let o piece = Occupied pos <| piece
        in case ch of 
                'p' -> o <| Black Pawn
                'n' -> o <| Black Knight
                'b' -> o <| Black Bishop
                'r' -> o <| Black Rook
                'q' -> o <| Black Queen
                'k' -> o <| Black King
                'P' -> o <| White Pawn
                'N' -> o <| White Knight
                'B' -> o <| White Bishop
                'R' -> o <| White Rook
                'Q' -> o <| White Queen
                'K' -> o <| White King
                _   ->      Vacant pos
