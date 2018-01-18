module View.Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Tuple exposing (..)
import Mouse exposing (..)
import Debug exposing (..)
import SvgParser exposing (parse)

import Data.Main exposing (..)
import Data.Game as G exposing (..)
import Settings exposing (..)
import Toolkit exposing (..)
import Time.Main as Time exposing (..)
import Notation.FEN as FEN exposing (..)
import View.Assets.Pieces exposing (..)

board : Model -> Html Msg
board model =
    div [ onMouseUp, onMouseDown ]
        [ renderBoard model ]

renderBoard : Model -> Html Msg 
renderBoard { game } =
    let chessboard = List.map renderRank game.board
    in div [class "board"] chessboard

renderRank : Rank -> Html Msg
renderRank r = div [class "rank"] (List.map renderSquare r)

renderSquare : Square -> Html Msg
renderSquare sq = case sq of
                     Vacant pos -> (tile pos) []
                     Occupied pos pc -> (tile pos) [renderPiece pos pc]

renderPiece : G.Position -> Piece -> Html Msg
renderPiece pos piece = 
    let pieceSvg : String
        pieceSvg = case piece of
                        Black fig -> getPiece "b_" fig
                        White fig -> getPiece "w_" fig
    -- parse SVG from String
    in case parse pieceSvg of
            Ok svg -> svg
            Err e  -> text e

tile : G.Position -> List (Html Msg) -> Html Msg
tile {x,y} = div []

--squareColor : Int -> Int -> String
--squareColor x y =
--    let black = isBlack x y
--    in if black 
--       then fst squareColors
--       else snd squareColors

--px : Int -> String
--px value = (toString value) ++ "px"

--isBlack : Int -> Int -> Bool
--isBlack x y = (rem (x + y) 2) == 0

--tile : G.Position -> List (Html Msg) -> Html Msg
--tile {x,y} = div []
            --[ style
            --[ 
            ----"position"        => "absolute"
            ----, "top"             => px (x * squareSize)
            ----, "left"            => px (y * squareSize)
            -- "width"           => px squareSize
            --, "height"          => px squareSize
            --, "backgroundColor" => squareColor x y
            --]
            --]
