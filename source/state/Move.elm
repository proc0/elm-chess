module State.Move exposing (..)

import Matrix exposing (..)
import Char exposing (..)
import Debug exposing (..)

import Mouse exposing (..)

import Data.Type exposing (..)
import Data.Tool exposing (..)
import Model.FEN exposing (..)
import State.Rules exposing (..)

updatePiecePos : Mouse.Position -> Maybe Piece -> Maybe Piece
updatePiecePos xy pc = Maybe.map (\p -> { p | position = xy }) pc

validate : Piece -> Board -> Board
validate piece board =
    let lc = toLocation <| fromMousePosition piece.position
        validMoves = pieceMoves piece board
        -- append input location as valid
        validLocations = lc::(List.map (\t -> t lc) validMoves)
        validateSquare sq = { sq | valid = True }
        validateSquares lc bd = 
            Matrix.update lc validateSquare bd
    in List.foldl validateSquares board validLocations

toSAN : Move -> String
toSAN move =
        let toNotation mv = 
            let ltr = figCharMap mv.piece.role
                (y1,x1) = mv.start
                --(x2,y2) = mv.end
            in if mv.piece.role /= Pawn
               then String.fromChar (if mv.piece.color == White then toUpper ltr else ltr) ++ (String.fromChar <| fromCode (x1 + 97)) ++ (toString <| 8-y1)
               else (String.fromChar <| fromCode (x1 + 97)) ++ (toString <| 8-y1)
        in toNotation move

