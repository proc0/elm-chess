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
import Frame.Main as Frame exposing (..)
import Notation.FEN as FEN exposing (..)
import View.Assets.Pieces exposing (..)

render : Model -> Html Msg
render model =
    node "main" 
        [ onMouseDown 
        ]
        [ r_pieces model
        , r_board model 
        ]

-- render pieces

r_pieces : Model -> Html Msg
r_pieces { game, position, drag } = 
    let pieces = map_pieces (get_piece drag) game.board
    in node "pieces" [] pieces

map_pieces : (Square -> Maybe (Html Msg)) -> Board -> List (Html Msg)
map_pieces f b = List.map (\r -> filter_squares f r) b |> List.concat

filter_squares : (Square -> Maybe (Html Msg)) -> Rank -> List (Html Msg)
filter_squares f r = List.filterMap (\s -> f s) r

get_piece : Maybe Drag -> Square -> Maybe (Html Msg)
get_piece dr sq =
    let dragPos ppos = 
        case dr of
             Just {start,current} -> current
             Nothing -> ppos
    in case sq of
            Occupied pos pc -> Just (r_piece (dragPos pos) pc)
            Vacant pos -> Nothing

r_piece : G.Position -> Piece -> Html Msg
r_piece {x,y} piece = 
    let pieceSvg : String
        pieceSvg = case piece of
                        Black fig -> getPiece "b_" fig
                        White fig -> getPiece "w_" fig
    -- parse SVG from String
    in case parse pieceSvg of
            Ok svg -> node "piece" 
                        [ style 
                            [ "top" => px x
                            , "left" => px y
                            ]
                        ] [svg]
            Err e -> text e

-- render board

r_board : Model -> Html Msg 
r_board { game } =
    let chessboard = List.map (r_rank r_square) game.board
    in node "board" [] chessboard

--r_rank : Rank -> Html Msg
--r_rank r = map_rank r_square r

r_rank : (Square -> Html Msg) -> Rank -> Html Msg
r_rank f r = node "rank" [] (List.map f r)

r_square : Square -> Html Msg
r_square _ = node "square" [] []
