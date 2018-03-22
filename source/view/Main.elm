module View.Main exposing (..)

import Matrix exposing (flatten, toList)
import Html exposing (Attribute, Html, div, node, text)
import Html.Attributes exposing (class, classList, style)
import Html.Events exposing (on)
import Mouse exposing (Position)
import SvgParser exposing (parse)
import Json.Decode as Json exposing (map)
import Material.Layout as Layout exposing (render, fixedHeader)
import Material.Options as Options
import Material.Color as Color
import Material.Typography as Typo
import Material.Table as Table
import Material.List as MList
import Debug exposing (log)

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
        debugPanel =
            if ui.debug
            then 
                [(node "pre" 
                [ class "debug-panel"
                ]
                [ text <| debugHistory chess.history
                ])]
            else 
                []     
    in 
    (Layout.render GUI ui.mdl [Layout.fixedHeader]
            { header = [
                  Options.div 
                    [ Typo.subhead
                    , Color.text (Color.white)
                    ] 
                    [ Options.div [] 
                        ([ text "Demo Chess"
                        ] 
                        ++ 
                        debugPanel)
                    ]
                ]
            , drawer = []
            , tabs = ([], [])
            , main = [
                r_game game
                , Options.div [ Options.cs "hud" ]
                    [ Options.div
                        [ Options.cs "player"
                        ]
                        [ text blackPlayer.name
                        ]
                    , Options.div 
                        [ Options.cs "history"
                        ] 
                        [ Options.div 
                            [ Typo.subhead
                            ] 
                            [ text ui.turn
                            ]
                        , MList.ul 
                            [ Options.cs "moves" 
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
                        [ Options.cs "player"
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
                Moving sel -> ([classList [("visible", True), (toString color, True)]], [r_dragSvg sel])
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
r_dragSvg : Selection -> Html Event
r_dragSvg { focus, piece } = 
    let x = piece.position.x
        y = piece.position.y
        o = toBoardPosition focus
        -- minus 56px from header
        newPos = Position (x - 32) ((y - 32) - 56)
        l_t = newPos.x < o.x - 18
        r_t = newPos.x > o.x + 18
        u_t = newPos.y < o.y - 18
        d_t = newPos.y > o.y + 18
        dest =
            if l_t || r_t || u_t || d_t
            then newPos
            -- not sure why the offset to 
            -- keep piece still on click
            else Position (o.x+4) (o.y+4)
    in 
    -- parse SVG from String
    case parse (getSvg piece) of
        Err e -> text e
        Ok svg -> node "piece" 
                    [ style 
                        [ "position" => "absolute"
                        , "top" => px dest.y
                        , "left" => px dest.x
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
    let attrs = 
            if sq.valid 
            then [classList [("active", sq.active), ("valid", sq.valid), ("selected", True)]] 
            else []
    in node "square" attrs []
