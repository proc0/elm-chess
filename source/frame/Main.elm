module Frame.Main exposing (..)

import Array exposing (..)
import Matrix exposing (..)
import Html exposing (..)
import Mouse exposing (..)
import Debug exposing (..)

import Data.Chess exposing (..)
import Frame.Moves exposing (..)
import Settings exposing (..)
import Toolkit exposing (..)

update : Msg -> Chess -> ( Chess, Cmd Msg )
update msg ({ board, player, history } as model) =
    let -- get selected square
        selected : Maybe Square
        selected = 
            let target p = Matrix.get (toLocation <| getPosition p) board
            in case msg of
                Click xy -> target xy
                Drag xy  -> target xy
                Drop xy  -> target xy
        -- update player action
        -- persist piece select + drag
        playerMove : Player
        playerMove = 
            case msg of 
                Click xy ->
                    case selected of
                        -- select/drag piece
                        Just sel -> 
                            { player 
                            | select = selected
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
                            | drag = Just { d | position = xy }
                            }
                        Nothing -> player

                Drop _  -> { player 
                           | drag = Nothing 
                           }

        -- calculate possible next move
        nextMove : Maybe Move
        nextMove = 
            let clear = toggleValid False
                defaults = clear board
                halfMove s = Just (s, Nothing)
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
                                    -- prevent same square drop
                                    in if tg.valid && tg.position /= sq.position
                                    -- complete move by drag
                                    then fullMove (sq => dest)
                                    else Nothing
                                Nothing -> Nothing
                        -- dead branch
                        Nothing -> Nothing                      
                -- move by click
                Click xy -> 
                    selected |> Maybe.map
                        -- check selected square
                        (\sel ->
                            -- if occupied by piece
                            case sel.piece of
                                -- player starts move
                                Just _ -> halfMove sel 
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
                                                    -- prevent same square click
                                                    in if tg.valid && tg.position /= sq.position
                                                    -- complete move by click (from, to)
                                                    then fullMove (sq => dest)
                                                    else Nothing
                                                ) |> Maybe.withDefault Nothing
                                        -- or invalid move
                                        ) |> Maybe.withDefault Nothing 
                        ) |> Maybe.withDefault Nothing

        -- calculate next board 
        -- based on player move
        nextBoard : Board
        nextBoard = 
            let clear = toggleValid False
                defaults = clear board
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
                                    defaults
                                    |> validate s2 
                                    |> liftPiece sq1 
                                    |> addPiece s2.position s2.piece 
                                    |> clear
                                -- if half move
                                -- highlight board and lift piece
                                Nothing -> 
                                    defaults 
                                    |> validate sq1 
                                    |> liftPiece sq1
                        Nothing -> defaults
                -- move by drag
                Drop xy ->
                    case nextMove of
                        Just (sq1, sq2) ->
                            case sq2 of
                                -- if full move 
                                -- add piece to board
                                Just s2 -> 
                                    let pc = s2.piece -- active false when dropped
                                           |> Maybe.map (\p -> { p | active = False })
                                    in defaults
                                        |> validate s2
                                        |> addPiece s2.position pc
                                        |> clear
                                Nothing -> defaults
                        Nothing -> 
                            -- invalid move, return piece back
                            -- using selection square
                            case player.select of
                                Just sq -> 
                                    let pc = sq.piece -- active true if no move drop
                                           |> Maybe.map (\p -> { p | active = True })
                                    in addPiece sq.position pc board
                                Nothing -> defaults
        -- update history based on next move
        nextHistory : History
        nextHistory = 
            case nextMove of
                Just ((sq1, sq2) as move) ->
                    case sq2 of
                        Just s2 -> history ++ [move]
                        Nothing -> history
                Nothing -> history

        _ = log "move" nextMove

    in Chess nextBoard playerMove nextHistory ! []
                                           

startDrag : Mouse.Position -> Square -> Square
startDrag ps sq = 
    case sq.piece of
            Just p -> 
                { sq 
                | position = ps
                , piece = Just ({ p | active = True })
                }
            Nothing -> sq

updateDrag : Mouse.Position -> Square -> Maybe Player -> Maybe Player
updateDrag xy sq player = 
    Maybe.map (\p -> 
        { p | drag = Just ({ sq | position = xy })
        }) player


-- board manipulations
----------------------

toggleValid : Bool -> Board -> Board
toggleValid isValid board=
        Matrix.map (\{position,piece} -> 
            --let newPiece = Maybe.map (\p -> { p | active = isValid }) piece
            Square position piece isValid) board

liftPiece : Square -> Board -> Board
liftPiece sq bd = 
    Matrix.update (toLocation sq.position) (\s -> { s | piece = Nothing }) bd

addPiece : Mouse.Position -> Maybe Piece -> Board -> Board
addPiece ps pc bd = 
    Matrix.update (toLocation ps) (\s -> 
        if s.valid 
        then { s | piece = pc, valid = True } 
        else s) bd

