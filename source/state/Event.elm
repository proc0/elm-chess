module State.Event exposing (..)

import Array exposing (..)
import Char exposing (..)
import Matrix exposing (..)
import Html exposing (..)
import Mouse exposing (..)
import Debug exposing (..)
import Material.Layout as Layout
import Material

import Data.Type exposing (..)
import Data.Tool exposing (..)
import Model.FEN exposing (..)
import Model.Board exposing (..)
import State.Move exposing (..)

subscriptions : Chess -> Sub Msg
subscriptions { player, ui } = 
            case player.drag of 
                Just sq ->
                    Sub.batch 
                        [ Mouse.moves Drag
                        , Mouse.ups Drop
                        , Layout.subs Mdl ui.mdl
                        ]                   
                Nothing -> Layout.subs Mdl ui.mdl

update : Msg -> Chess -> ( Chess, Cmd Msg )
update msg 
    ({ board
     , player
     , history
     , ui 
     } 
     as model) =

    let -- get focus square
        focus : Maybe Square
        focus = 
            let target p = Matrix.get (toLocation <| getPosition p) board
            in mapMsg target msg
            
        -- calculate next turn color 
        -- from last move
        nextTurn : Color
        nextTurn = 
            let lastMove = List.head history 
            in case lastMove of
                Just (_, mv) ->
                    case mv of
                        Just m -> White
                        Nothing -> Black
                Nothing -> White

        -- update player action
        -- persist piece select + drag
        nextPlayer : Player
        nextPlayer = 
            case msg of 
                Click xy ->
                    case focus of
                        -- select/drag piece
                        Just sel -> 
                            { player 
                            | select = focus
                            , drag   = Just (startDrag xy sel) 
                            }
                        -- clear selection
                        Nothing -> 
                            { player 
                            | select = Nothing
                            , drag   = Nothing 
                            }

                Drag xy -> 
                    case player.drag of
                        Just d -> 
                            { player 
                            | drag = Just { d | point = xy }
                            }
                        Nothing -> player

                Drop _  -> { player 
                           | drag = Nothing 
                           }
                _ -> player


        -- calculate possible next move
        nextMove : Maybe Move
        nextMove = 
            let clear = toggleValid False
                defaults = clear board
                startMove s = Just (s, Nothing)
                fullMove (s1,s2) = Just (s1, Just s2)
                findTarget ps sq = 
                    Matrix.get 
                        (toLocation ps) 
                        (validate sq defaults)
            in case msg of
                -- no move update when dragging
                Drag _  -> Nothing
                -- move by drag
                Drop xy ->
                    -- if existing selection
                    case player.select of
                        Just sq -> 
                            let pos = getPosition xy
                            -- check target square 
                            in case (findTarget pos sq) of
                                Just tg ->
                                    let dest = Square pos sq.piece True
                                        rightColor = 
                                            case sq.piece of
                                                Just pc -> pc.color == nextTurn
                                                Nothing -> False
                                    -- prevent same square drop
                                    in if tg.valid && tg.point /= sq.point && rightColor
                                    -- complete move by drag
                                    then fullMove (sq => dest)
                                    else Nothing
                                Nothing -> Nothing
                        -- dead branch
                        Nothing -> Nothing                      
                -- move by click
                Click xy -> 
                    focus |> Maybe.map
                        -- check focus square
                        (\sel ->
                            -- if occupied by piece
                            case sel.piece of
                                -- player starts move
                                Just _ -> startMove sel 
                                Nothing -> 
                                    -- check if existing selection
                                    player.select |> Maybe.map 
                                        -- if player has selection
                                        -- then square click is a move                                            
                                        (\sq ->
                                            let pos = getPosition xy
                                            -- check clicked target square
                                            in (findTarget pos sq) |> Maybe.map
                                                (\tg ->
                                                    let dest = Square pos sq.piece True
                                                        rightColor = 
                                                            case sq.piece of
                                                                Just pc -> pc.color == nextTurn
                                                                Nothing -> False
                                                    -- prevent same square click
                                                    in if tg.valid && tg.point /= sq.point && rightColor
                                                    -- complete move by click (from, to)
                                                    then fullMove (sq => dest)
                                                    else Nothing
                                                ) |> Maybe.withDefault Nothing
                                        -- or invalid move
                                        ) |> Maybe.withDefault Nothing 
                        ) |> Maybe.withDefault Nothing
                _ -> Nothing

        -- calculate next board 
        -- based on player move
        nextBoard : Board
        nextBoard = 
            let clear = toggleValid False

                default : Board
                default = clear board

                startMove : Square -> Board
                startMove s1 = default 
                        |> validate s1 
                        |> liftPiece s1

                fullMove : Square -> Square -> Board
                fullMove s1 s2 =
                        let pc = s2.piece -- active false when dropped
                           |> Maybe.map (\p -> { p | active = False, moved = True })                    
                        in default
                            |> validate s2 
                            |> liftPiece s1 
                            |> addPiece s2.point pc 
                            |> clear

                finishMove : Square -> Board
                finishMove s2 =
                    let pc = s2.piece -- active false when dropped
                           |> Maybe.map (\p -> { p | active = False, moved = True })
                    in default
                        |> validate s2
                        |> addPiece s2.point pc
                        |> clear

                undoMove : Square -> Board
                undoMove s =
                    let pc = s.piece -- active true if no move drop
                           |> Maybe.map (\p -> { p | active = True })
                    in addPiece s.point pc board

            in case msg of
                -- no update
                Drag _  -> board
                -- move by click
                Click xy -> 
                    case nextMove of
                        Just (sq1, sq2) -> 
                            -- if full move, translate piece 
                            -- and clear board highilight
                            case sq2 of
                                Just s2 -> 
                                    case s2.piece of
                                        Just _ -> fullMove sq1 s2
                                        Nothing -> startMove sq1
                                -- if half move
                                -- highlight board and lift piece
                                Nothing -> startMove sq1
                                    
                        Nothing -> default
                -- move by drag
                Drop xy ->
                    case nextMove of
                        Just (sq1, sq2) ->
                            case sq2 of
                                -- if full move 
                                -- add piece to board
                                Just s2 -> finishMove s2
                                Nothing -> default
                        Nothing -> 
                            -- invalid move, return piece back
                            -- using selection square
                            case player.select of
                                Just sq -> undoMove sq
                                Nothing -> default
                _ -> board
                
        -- update history based on next move
        nextHistory : History
        nextHistory = 
            let lastMove = List.head history
                hist = let rev = List.tail history
                        in Maybe.withDefault [] rev
            in case lastMove of
                Just (s1_,s2_) -> 
                    case nextMove of
                        Just ((sq1, sq2) as move) ->
                            case sq2 of
                                Just s2 -> 
                                    if sq1 /= s2 
                                    then (case s2_ of
                                        Just s -> (s2, Nothing)::history
                                        Nothing -> (s1_, sq2)::hist)
                                    else history
                                Nothing -> history
                        Nothing -> history
                Nothing -> 
                    case nextMove of
                        Just ((sq1, sq2) as move) ->
                            case sq2 of
                                Just s2 -> 
                                    if sq1 /= s2
                                    then [(s2, Nothing)]
                                    else []
                                Nothing -> []
                        Nothing -> []

        nextUI : UI 
        nextUI =
            if nextTurn == White
            then { ui | turn = "White's turn"}
            else { ui | turn = "Black's turn"}

    in case msg of
        Mdl message -> 
            let (nextMaterial, subMsg) = Material.update Mdl message nextUI
            in Chess board player history nextMaterial ! [subMsg]                         
        _ -> 
            Chess nextBoard nextPlayer nextHistory nextUI ! []
