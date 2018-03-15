module Model.FEN exposing (..)

import Regex
import String
import Char exposing (..)
import Array exposing (..)
import Matrix exposing (..)
import Debug exposing (..)

import Data.Type exposing (..)
import Data.Tool exposing (..)

--             Forsythe Edwards Notation (FEN)             --
--=========================================================--

initialPieces = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
initialBoard = initialPieces ++ " w KQkq - 0 1"

-- other examples
--testingEngine = "K6k/3n2pp/8/1P6/B7/8/6PP/8 w - - 0 1"
--whiteInCheck = "rnb1kbnr/pppppppp/4q3/8/8/8/PPP2PPP/RNBQKBNR w KQkq - 0 1"
--blackCheckMate = "1R6/8/kQ6/8/8/8/6K1/8 w - - 0 1"
--enPassantBoard = "rnbqkbnr/1ppppppp/8/pP6/8/8/P1PPPPPP/RNBQKBNR w KQkq a6 0 1"
--castlingAvailable = "rnbqkbnr/1ppppppp/8/pP6/8/8/P1PPPPPP/R3K2R w KQkq - 0 1"

fromFEN : String -> Chess
fromFEN fen =
    let parts =
        String.split " " fen |> Array.fromList
        -- TODO: update state when initial FEN has en passant flag
        --hasEnPassant = Maybe.withDefault "-" (Array.get 3 parts)
        board = parsePieces (Maybe.withDefault initialPieces (Array.get 0 parts))
        history = []
    in
    Chess board history
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
    String.split "/" s |> List.map2 toRank boardside |> Matrix.fromList

toRank : Int -> String -> Rank
toRank y row =
    let pieces = expand row 
                 |> String.toList
        points = List.map2 (,) boardside (List.repeat 8 y)
                 |> List.map swap |> List.map (uncurry loc)
    in List.map2 toSquare points pieces

expand : String -> String
expand s =
    Regex.replace Regex.All (Regex.regex "[12345678]") expandMatch s

expandMatch { match } =
    String.repeat (Result.withDefault 0 (String.toInt match)) " "

toSquare : Location -> Char -> Square
toSquare lc ch = 
    let vacant = Square lc Nothing False False
        occupied = Square lc (Just <| toPiece ch lc) False False
    in 
    if toRole ch == Ninja -- empty square sentinel
    then vacant           -- guards against invalid 
    else occupied         -- piece letters

toPiece : Char -> Location -> Piece
toPiece ch location =
    let position = 
            toBoardPosition location
        -- initial location
        path = [location]
        role = toRole ch
        tick = 0
        newPiece color = 
            (Piece 
                position 
                location 
                color 
                role 
                tick
                path)
    in 
    if isUpper ch
    then newPiece White
    else newPiece Black

toRole : Char -> Role
toRole ch =
    case (toLower ch) of 
        'p' -> Pawn
        'n' -> Knight
        'b' -> Bishop
        'r' -> Rook
        'q' -> Queen
        'k' -> King
        _   -> Ninja

fromRole : Role -> Char
fromRole fig = 
    case fig of
        Pawn    -> 'p'
        Rook    -> 'r'
        Bishop  -> 'b'
        Knight  -> 'n'
        Queen   -> 'q'
        King    -> 'k'
        Ninja   -> 'j'
