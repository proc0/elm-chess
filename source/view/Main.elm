module View.Main exposing (..)

import Char exposing (..)
import Matrix exposing (..)
import Maybe.Extra as Maebe exposing (..)
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
import Material.Typography as Typo
import Material.Table as Table
import Material.List as MList
import Material.Options as Options

import Data.Type exposing (..)
import Data.Tool exposing (..)
import Model.FEN exposing (..)
import Model.History exposing (..)
import View.Asset exposing (..)

onMouseDown : Attribute Event
onMouseDown = on "mousedown" (Json.map Click Mouse.position)

render : Game -> Html Event
render ({ ui, chess, players } as game) =
    let whitePlayer = getWhite chess.history players
        blackPlayer = getBlack chess.history players
    in 
    (Layout.render Mdl ui.mdl [Layout.fixedHeader]
            { header = [
                  Options.div 
                    [ Typo.subhead
                    , Color.text (Color.white)
                    ] 
                    [ text "Demo Chess" ]
                ]
            , drawer = []
            , tabs = ([], [])
            , main = [
                r_game game
                , Options.div [ cs "hud" ]
                    [ Options.div
                        [ cs "player"
                        ]
                        [ text blackPlayer.name
                        ]
                    , Options.div 
                        [ cs "history"
                        ] 
                        [ Options.div 
                            [ Typo.subhead
                            ] 
                            [ text ui.turn
                            ]
                        , MList.ul 
                            [ cs "moves" 
                            ]
                            (chess.history 
                                |> formatHistory
                                |> List.map (\move ->
                                    MList.li 
                                        [ -- 
                                        ] 
                                        [ text move 
                                        ]
                                )
                            )
                        ]
                    , Options.div
                        [ cs "player"
                        ]
                        [ text whitePlayer.name
                        ]
                    ]
                ]
            })

-- render board and pieces
r_game : Game -> Html Event
r_game { chess, players } =
        node "chess" 
            [ onMouseDown,
              class <| String.toLower <| toString (fst players).color
            ]
            [ r_player (fst players)
            , r_pieces chess.board
            , r_board chess.board 
            ]

-- render player layer
----------------------
r_player : Player -> Html Event
r_player {color, action} =
    let mv_piece = 
            case action of
                Moving sel -> ([classList [("visible", True), (toString color, True)]], [r_dragSvg sel.piece])
                _ -> ([], [])
    in (uncurry (node "player")) mv_piece

-- render pieces
----------------
r_pieces : Board -> Html Event
r_pieces board = 
    let sortSqs = 
            List.foldl (\sq mem -> 
                case sq.piece of
                    Just pc ->
                        case pc.color of
                            Black -> (fst mem, snd mem ++ [sq])
                            White -> (fst mem ++ [sq], snd mem)
                    _ -> mem) ([], [])
        pieces = map_board r_piece <| sortSqs <| Matrix.flatten board
    in node "pieces" [] 
        [ div [class "white"] (fst pieces)
        , div [class "black"] (snd pieces)
        ]

map_board : (Square -> Maybe (Html Event)) -> (Rank, Rank) -> (List (Html Event), List (Html Event))
map_board f (rw, rb) = 
    (filter_rank f rw, filter_rank f rb)

-- empty squares will be filtered
filter_rank : (Square -> Maybe (Html Event)) -> Rank -> List (Html Event)
filter_rank f r = List.filterMap (\s -> f s) r

-- render piece from square
r_piece : Square -> Maybe (Html Event)
r_piece s = case s.piece of
                 Just pc -> Just (r_svg pc)
                 Nothing -> Nothing

-- render svg piece
r_svg : Piece -> Html Event
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
r_dragSvg : Piece -> Html Event
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
r_board : Board -> Html Event 
r_board board =
    let checker = List.map (r_rank r_square) (Matrix.toList board)
    in node "board" [] checker

r_rank : (Square -> Html Event) -> Rank -> Html Event
r_rank f r = node "rank" [] (List.map f r)

r_square : Square -> Html Event
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
