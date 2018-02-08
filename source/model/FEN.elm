module Model.FEN exposing (..)

import Regex
import String
import Char exposing (..)
import Array exposing (..)
import Matrix exposing (..)
import Debug exposing (..)

import Data.Type exposing (..)
import Data.Tool exposing (..)

-- Forsythe Edwards Notation (FEN) -> Board

initialPieces = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
initialBoard = initialPieces ++ " w KQkq - 0 1"

-- examples

--testingEngine = "K6k/3n2pp/8/1P6/B7/8/6PP/8 w - - 0 1"
--whiteInCheck = "rnb1kbnr/pppppppp/4q3/8/8/8/PPP2PPP/RNBQKBNR w KQkq - 0 1"
--blackCheckMate = "1R6/8/kQ6/8/8/8/6K1/8 w - - 0 1"
--enPassantBoard = "rnbqkbnr/1ppppppp/8/pP6/8/8/P1PPPPPP/RNBQKBNR w KQkq a6 0 1"
--castlingAvailable = "rnbqkbnr/1ppppppp/8/pP6/8/8/P1PPPPPP/R3K2R w KQkq - 0 1"

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
    in List.map2 toSquare posons pieces

expand : String -> String
expand s =
    Regex.replace Regex.All (Regex.regex "[12345678]") expandMatch s

expandMatch { match } =
    String.repeat (Result.withDefault 0 (String.toInt match)) " "

toSquare : Point -> Char -> Square
toSquare pt ch = 
    let vacant = Square pt Nothing False False
        occupied = Square pt (Just <| toPiece ch pt) False False
    in if charFigMap ch == Zebra -- empty square sentinel
        then vacant              -- guards against invalid 
        else occupied            -- piece letters

toPiece : Char -> Point -> Piece
toPiece ch pt =
    let white rl = Piece White rl pt False
        black rl = Piece Black rl pt False
        role = charFigMap ch
    in if isUpper ch
        then white role
        else black role

charFigMap : Char -> Role
charFigMap ch =
            case (toLower ch) of 
                'p' -> Pawn
                'n' -> Knight
                'b' -> Bishop
                'r' -> Rook
                'q' -> Queen
                'k' -> King
                _   -> Zebra

figCharMap : Role -> Char
figCharMap fig = 
            case fig of
                Pawn    -> 'p'
                Rook    -> 'r'
                Bishop  -> 'b'
                Knight  -> 'n'
                Queen   -> 'q'
                King    -> 'k'
                Zebra   -> 'z'
