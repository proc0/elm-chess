module View.Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Mouse exposing (..)
import Matrix exposing (..)
import SvgParser exposing (parse)
import Debug exposing (..)

import Data.Main exposing (..)
import Data.Game as G exposing (..)
import Settings exposing (..)
import Toolkit exposing (..)
import Frame.Main exposing (..)
import View.Assets.Pieces exposing (..)

render : Chess -> Html Msg
render { board, player } =
    node "main" 
        [ onMouseDown 
        ]
        [ r_player player
        , r_pieces board
        , r_board board 
        ]

-- render player layer
----------------------
r_player : Player -> Html Msg
r_player {select, drag} =
    let mv_piece =
        case drag of
            Just d -> ([class "visible"], [r_dragSvg d])
            Nothing -> ([], [])        
    in (uncurry (node "player")) mv_piece

-- render pieces
----------------
r_pieces : Board -> Html Msg
r_pieces board = 
    let pieces = map_board r_piece (Matrix.toList board)
    in node "pieces" [] pieces

map_board : (Square -> Maybe (Html Msg)) -> List Rank -> List (Html Msg)
map_board f b = 
    List.map (\r -> filter_rank f r) b |> List.concat

-- empty squares will be filtered
filter_rank : (Square -> Maybe (Html Msg)) -> Rank -> List (Html Msg)
filter_rank f r = List.filterMap (\s -> f s) r

-- render piece from square
r_piece : Square -> Maybe (Html Msg)
r_piece s = case s.piece of
                 Just pc -> Just (r_svg s.position pc)
                 Nothing -> Nothing

-- render svg piece
r_svg : G.Position -> Piece -> Html Msg
r_svg {x,y} piece = 
    -- parse SVG from String
    case parse (getPieceSvgPrefix piece) of
        Err e -> text e
        Ok svg -> node "piece" 
                    [ style 
                        [ "top" => px (y * squareSize)
                        , "left" => px (x * squareSize)
                        ]
                    ] [svg]

-- dragable svg markup
r_dragSvg : Square -> Html Msg
r_dragSvg {position,piece} = 
    let x = position.x
        y = position.y
    in case piece of
        Just pc ->
            -- parse SVG from String
            case parse (getPieceSvgPrefix pc) of
                Err e -> text e
                Ok svg -> node "piece" 
                            [ style 
                                [ "position" => "absolute"
                                , "top" => px (y - 32)
                                , "left" => px (x - 32)
                                ]
                            ] [svg]
        Nothing -> text "" --neutral

getPieceSvgPrefix : Piece -> String
getPieceSvgPrefix  {color, role} = 
    case color of
        Black -> getSvgTag "b_" role
        White -> getSvgTag "w_" role

-- render board
---------------
r_board : Board -> Html Msg 
r_board board =
    let checker = List.map (r_rank r_square) (Matrix.toList board)
    in node "board" [] checker

r_rank : (Square -> Html Msg) -> Rank -> Html Msg
r_rank f r = node "rank" [] (List.map f r)

r_square : Square -> Html Msg
r_square sq = 
    let attrs = 
        if sq.valid 
        then [class "selected"] 
        else []
    in node "square" attrs []
