module View.Main exposing (..)

import Matrix exposing (..)
import Char exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Mouse exposing (..)
import SvgParser exposing (parse)
import Json.Decode as Json exposing (..)
import Debug exposing (..)

import Material.Layout as Layout
import Material.Options as Options exposing (css, cs, when)
import Material.Color as Color
import Material.Typography as Typography
import Material.Table as Table
import Material.Options as Options

import Data.Type exposing (..)
import Data.Tool exposing (..)
import Model.FEN exposing (..)
import State.Move exposing (..)
import View.Asset exposing (..)

onMouseDown : Attribute Msg
onMouseDown = on "mousedown" (Json.map Click Mouse.position)

render : Game -> Html Msg
render ({ ui, board, history } as game) =
    (Layout.render Mdl ui.mdl [Layout.fixedHeader]
            { header = [
                  Options.div 
                    [ Typography.title
                    , Color.text (Color.white)
                    ] 
                    [ text ui.turn ]
                ]
            , drawer = []
            , tabs = ([], [])
            , main = [
                r_game game
                , Options.div [ cs "hud" ] [ 
                    Table.table []
                        (List.reverse history 
                            |> List.map (\mv ->
                                Table.tr [] [ Table.td [] [ text <| toSAN mv ]])
                        )
                    ]
                ]
            })

-- render board and pieces
r_game : Game -> Html Msg
r_game { board, players } =
        node "chess" 
            [ onMouseDown 
            ]
            [ r_player (fst players)
            , r_pieces board
            , r_board board 
            ]

-- render player layer
----------------------
r_player : Player -> Html Msg
r_player {color, moving} =
    let mv_piece =
        case moving of
            Just pc -> ([classList [("visible", True), (toString color, True)]], [r_dragSvg pc])
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
                 Just pc -> Just (r_svg pc)
                 Nothing -> Nothing

-- render svg piece
r_svg : Piece -> Html Msg
r_svg ({ position } as piece) = 
    let {x,y} = position
        --_ = log "coors" (x,y)
        styles = 
            [style 
                [ "top" => px y
                , "left" => px x
                ]
            ]
    -- parse SVG from String
    in case parse (getSvg piece) of
        Err e -> text e
        Ok svg -> node "piece" styles [svg]

-- dragable svg markup
r_dragSvg : Piece -> Html Msg
r_dragSvg ({ position } as piece) = 
    let x = position.x
        y = position.y
    in -- parse SVG from String
        case parse (getSvg piece) of
            Err e -> text e
            Ok svg -> node "piece" 
                        [ style 
                            [ "position" => "absolute"
                            -- minus 56px from header
                            , "top" => px ((y - 32) - 56)
                            , "left" => px (x - 32)
                            ]
                        ] [svg]

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
    let activeClass = 
            if sq.active
            then "active"
            else "selected"
        attrs = 
            if sq.valid 
            then [class activeClass] 
            else []
    in node "square" attrs []
