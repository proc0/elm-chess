module View.Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Tuple exposing (..)
import Mouse exposing (..)
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
    div [ onMouseUp ]
        [ renderBoard model ]

renderBoard : Model -> Html Msg 
renderBoard model =
    let chessboard = List.map2 rank (List.range 0 7) model.game.board |> List.concat
    in div [] chessboard

rank : Int -> Rank -> List (Html Msg)
rank x r = List.map2 (tile x) (List.range 0 7) r

tile : Int -> Int -> Square -> Html Msg
tile x y sq =
    let xStr = (x * squareSize) |> px
        yStr = (y * squareSize) |> px
        bgColor = squareColor x y
        bgTile = div
            [ style
                [ "position"        => "absolute"
                , "top"             => xStr
                , "left"            => yStr
                , "width"           => px squareSize
                , "height"          => px squareSize
                , "backgroundColor" => bgColor
                ]
            ]
    in case sq of
            Vacant -> bgTile []
            Occupied p -> bgTile [piece x y p]

piece : Int -> Int -> Piece -> Html Msg
piece x y p = let 
    getPiece : String -> G.Figure -> String
    getPiece c f = getPieceSrc (c ++ String.fromChar (getPieceChar f))
    pieceSrc = 
        case p of
             Black f -> getPiece "b_" f
             White f -> getPiece "w_" f
              -- parse SVG from String
              in case (parse pieceSrc) of
                      Ok svg -> svg
                      Err e  -> text e

squareColor : Int -> Int -> String
squareColor x y =
    let black = isBlack x y
    in if black 
       then fst squareColors
       else snd squareColors

px : Int -> String
px value = (toString value) ++ "px"

isBlack : Int -> Int -> Bool
isBlack x y = (rem (x + y) 2) == 0