module State.Event exposing (..)

import Array exposing (..)
import Char exposing (..)
import Matrix exposing (..)
import Html exposing (..)
import Mouse exposing (..)
import Debug exposing (..)
import Maybe.Extra as Maebe exposing (..)
import Material.Layout as Layout
import Material

import Data.Type exposing (..)
import Data.Tool exposing (..)
import Model.FEN exposing (..)
import Model.Board exposing (..)
import State.Move exposing (..)

subscriptions : Game -> Sub Msg
subscriptions { ui, players, turn } = 
            let dragSub = 
                    Sub.batch 
                        [ Mouse.moves Drag
                        , Mouse.ups Drop
                        , Layout.subs Mdl ui.mdl
                        ]                              
            in case turn.moving of
                Just moving -> 
                    case moving of
                        Touch pc -> dragSub
                        Lift pc -> dragSub
                        End mv -> Layout.subs Mdl ui.mdl
                        Pass -> Sub.none
                Nothing -> Layout.subs Mdl ui.mdl

update : Msg -> Game -> ( Game, Cmd Msg )
update msg 
    ({ ui
     , players
     , board
     , turn
     , history
     } 
     as game) =

    let -- get selected square
        selected : Maybe Square
        selected = 
            let target p = Matrix.get (toLocation <| getPosition p) board
            in msg |> mapMsg (\xy -> 
                let tg = target xy
                in Maybe.map (\t -> 
                        Maybe.map (always t) t.piece) 
                    tg) |> Maebe.join

        -- calculate possible next move
        nextMove : Maybe Move
        nextMove = 
            let defaults = clear board
                startMove s = Nothing
                --startMove s = Just (s, Nothing)
                fullMove (s1,s2) = 
                    Maybe.map (\p -> Move p s1.point s2.point) s1.piece 
                    --Just (s1, Just s2)
                findTarget ps mv = 
                    Matrix.get 
                        (toLocation ps) 
                        (validate mv defaults)
            in case msg of
                -- no move update when dragging
                Drag _  -> Nothing
                -- move by drag
                Drop xy ->
                    -- if existing selection
                    case turn.select of
                        Just sq -> 
                            let pos = getPosition xy
                                zeb = Piece White Zebra pos False
                                mov = Move (sq.piece ? zeb) pos pos
                            -- check target square 
                            in case (findTarget pos mov) of
                                Just tg ->
                                    let dest = Square tg.point sq.piece True False
                                        rightTurn = 
                                            case sq.piece of
                                                Just pc -> pc.color == turn.player.color
                                                Nothing -> False
                                    -- prevent same square drop
                                    in if tg.valid && tg.point /= sq.point && rightTurn
                                    -- complete move by drag
                                    then fullMove (sq => dest)
                                    else Nothing
                                Nothing -> Nothing
                        -- dead branch
                        Nothing -> Nothing                      
                -- move by click
                Click xy -> 
                    selected |> Maybe.map
                        -- check focus square
                        (\sel ->
                            -- if occupied by piece
                            case sel.piece of
                                -- player starts move
                                Just _ -> startMove sel 
                                Nothing -> 
                                    -- check if square focused
                                    turn.select |> Maybe.map 
                                        -- if player has selection
                                        -- then square click is a move                                            
                                        (\sq ->
                                            let pos = getPosition xy
                                                zeb = (Piece White Zebra pos False)
                                                mov = Move (sq.piece ? zeb) pos pos
                                            -- check clicked target square
                                            in (findTarget pos mov) |> Maybe.map
                                                (\tg ->
                                                    let dest = Square tg.point sq.piece True False
                                                        rightTurn = 
                                                            case sq.piece of
                                                                Just pc -> pc.color == turn.player.color
                                                                Nothing -> False
                                                    -- prevent same square click
                                                    in if tg.valid && tg.point /= sq.point && rightTurn
                                                    -- complete move by click (from, to)
                                                    then fullMove (sq => dest)
                                                    else Nothing
                                                ) |> Maybe.withDefault Nothing
                                        -- or invalid move
                                        ) |> Maybe.withDefault Nothing 
                        ) |> Maybe.withDefault Nothing
                _ -> Nothing


        nextTurn : Turn
        nextTurn = 
            let clickPiece xy tn =
                    case selected of
                        -- select/drag piece
                        Just sel -> 
                            { tn 
                            | select = selected
                            , moving = startDrag xy sel 
                            }
                        -- clear selection
                        Nothing -> 
                            case turn.select of
                                Just sl ->
                                    case sl.piece of
                                        Just pc ->
                                            { tn
                                            | moving = Just <| End (Move pc sl.point (getPosition xy))
                                            }
                                        Nothing -> 
                                            { tn 
                                            | select = Nothing
                                            , moving = Nothing 
                                            }                                            
                                Nothing ->
                                    { tn 
                                    | select = Nothing
                                    , moving = Nothing 
                                    }
                dragPiece xy tn =
                    let updateDrag = 
                        case tn.moving of
                            Just mv ->
                                case mv of
                                    Touch pc -> Just <| Lift { pc | point = xy }
                                    Lift pc  -> Just <| Lift { pc | point = xy }
                                    End m    -> Just <| End m
                                    Pass     -> Just <| Pass
                            Nothing -> Nothing
                    in { tn 
                       | moving = updateDrag 
                       }
            in case msg of 
                Click xy -> clickPiece xy turn
                Drag xy  -> dragPiece xy turn
                Drop _   -> 
                    case nextMove of
                        Just mv ->
                            { turn 
                            | select = Nothing
                            , moving = Just (End mv) 
                            }
                        Nothing ->
                            { turn 
                            | moving = Nothing 
                            }                            
                _ -> turn

        -- calculate next board 
        -- based on player move
        nextBoard : Board
        nextBoard = 
            let clear = toggleValid False

                default : Board
                default = clear board

                start : Move -> Board
                start move = default 
                        |> validate move 
                        |> liftPiece move

                fullMove : Move -> Board
                fullMove move = default
                        |> validate move
                        |> liftPiece move 
                        |> addPiece move
                        |> clear

                undoMove : Move -> Board
                undoMove move = returnPiece move board |> clear

            in case msg of
                -- no update
                Drag _  -> board
                -- move by click
                Click _ -> selected 
                        |> Maybe.map (\sel ->
                                case sel.piece of
                                    Just p -> start (Move p sel.point sel.point)
                                    Nothing -> default 
                            )
                        |> Maybe.withDefault default
                -- drop after drag
                Drop _ -> 
                    case nextTurn.moving of
                        Just moving -> 
                            case moving of
                                End move -> fullMove move
                                _ -> default
                        Nothing -> 
                            case turn.select of
                                Just sl -> 
                                    case sl.piece of
                                        Just pc -> undoMove (Move pc sl.point sl.point)
                                        Nothing -> default
                                Nothing -> default
                -- do nothing
                _ -> board

        -- update history based on next move
        nextHistory : History
        nextHistory = history
            --let lastMove = List.head history
            --    hist = let rev = List.tail history
            --            in Maybe.withDefault [] rev
            --in case lastMove of
            --    Just (s1_,s2_) -> 
            --        case nextMove of
            --            Just ((sq1, sq2) as move) ->
            --                case sq2 of
            --                    Just s2 -> 
            --                        if sq1 /= s2 
            --                        then (case s2_ of
            --                            Just s -> (s2, Nothing)::history
            --                            Nothing -> (s1_, sq2)::hist)
            --                        else history
            --                    Nothing -> history
            --            Nothing -> history
            --    Nothing -> 
            --        case nextMove of
            --            Just ((sq1, sq2) as move) ->
            --                case sq2 of
            --                    Just s2 -> 
            --                        if sq1 /= s2
            --                        then [(s2, Nothing)]
            --                        else []
            --                    Nothing -> []
            --            Nothing -> []
        nextUI : UI
        nextUI =
            if turn.player.color == White
            then { ui | turn = "White's turn"}
            else { ui | turn = "Black's turn"}
        --_ = log "log" nextPlayers
    in case msg of
        Mdl message -> 
            let (nextMaterial, subMsg) = Material.update Mdl message nextUI
            in Game nextMaterial players board turn history ! [subMsg]                         
        _ -> 
            Game nextUI players nextBoard nextTurn nextHistory ! []
