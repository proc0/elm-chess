module View.Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Tuple exposing (..)
import Mouse exposing (..)
import SvgParser exposing (parse)

import Data.Main exposing (..)
import Data.Game exposing (..)
import Settings exposing (..)
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

piece : Int -> Int -> Html Msg
piece _ _ = let k1 = parse w_K
            in case k1 of
                    Ok p -> p
                    Err e -> text e

tile : Int -> Int -> Square -> Html Msg
tile xPos yPos sq =
    let xStr = (xPos * squareSize) |> px
        yStr = (yPos * squareSize) |> px
        backgroundColor = squareColor xPos yPos
        sqTile = div
            [ style
                [ "position"        => "absolute"
                , "top"             => xStr
                , "left"            => yStr
                , "width"           => px squareSize
                , "height"          => px squareSize
                , "backgroundColor" => backgroundColor
                ]
            ]
    in case sq of
            Vacant -> sqTile []
            Occupied c p -> sqTile [piece xPos yPos]

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