module View.Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import SvgParser exposing (parse)
import Debug exposing (..)

import Data.Main exposing (..)
import Data.Game as G exposing (..)
import Settings exposing (..)
import Toolkit exposing (..)
import Frame.Main as Frame exposing (..)
import View.Assets.Pieces exposing (..)

render : Model -> Html Msg
render { game } =
    node "main" 
        [ onMouseDown 
        ]
        [ r_pieces game.board
        , r_board game.board 
        ]

-- render pieces
----------------
r_pieces : Board -> Html Msg
r_pieces board = 
    let pieces = map_board r_piece board
    in node "pieces" [] pieces

map_board : (Square -> Maybe (Html Msg)) -> Board -> List (Html Msg)
map_board f b = List.map (\r -> filter_rank f r) b |> List.concat

-- empty squares will be filtered
filter_rank : (Square -> Maybe (Html Msg)) -> Rank -> List (Html Msg)
filter_rank f r = List.filterMap (\s -> f s) r

-- render piece from square
r_piece : Square -> Maybe (Html Msg)
r_piece s = case s of
                 Occupied ps pc -> Just (r_svg ps pc)
                 Vacant ps -> Nothing

-- render svg piece
r_svg : G.Position -> Piece -> Html Msg
r_svg {x,y} piece = 
    let svgTag : String
        svgTag = case piece of
                      Black figure -> getSvgTag "b_" figure
                      White figure -> getSvgTag "w_" figure
    -- parse SVG from String
    in case parse svgTag of
            Err e -> text e
            Ok svg -> node "piece" 
                        [ style 
                            [ "top" => px x
                            , "left" => px y
                            ]
                        ] [svg]

-- render board
---------------
r_board : Board -> Html Msg 
r_board board =
    let checker = List.map (r_rank r_square) board
    in node "board" [] checker

r_rank : (Square -> Html Msg) -> Rank -> Html Msg
r_rank f r = node "rank" [] (List.map f r)

r_square : Square -> Html Msg
r_square _ = node "square" [] []
