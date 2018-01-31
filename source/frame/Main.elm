module Frame.Main exposing (..)

import Array exposing (..)
import Matrix exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Mouse exposing (..)
import Json.Decode as Json exposing (..)
import Debug exposing (..)

import Data.Main exposing (..)
import Data.Game as Game exposing (..)
import Frame.Moves exposing (..)
import Settings exposing (..)
import Toolkit exposing (..)

onMouseDown : Attribute Msg
onMouseDown = on "mousedown" (Json.map Click Mouse.position)

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
                -- perform move by click
                Click xy -> 
                    case selected of
                        -- check selected square
                        Just sq -> 
                            -- if occupied by piece
                            case sq.piece of
                                -- player starts move
                                Just pc -> halfMove sq 
                                Nothing -> 
                                    -- check if existing selection
                                    case player.select of
                                        -- if player has selection
                                        -- then square click is a move
                                        Just sq -> 
                                            let pos = getPosition xy
                                            -- check clicked target square
                                            in case (findTarget pos sq) of
                                                Just tg ->
                                                    -- prevent same square click
                                                    if tg.valid && tg.position /= sq.position
                                                    -- complete move by click (from, to)
                                                    then fullMove (sq => Square pos sq.piece True)
                                                    else Nothing
                                                -- invalid target
                                                Nothing -> Nothing
                                        Nothing -> Nothing 
                        Nothing -> Nothing
                -- move by drag
                Drop xy ->
                    -- if existing selection
                    case player.select of
                        Just sq -> 
                            let pos = getPosition xy
                            -- check target square 
                            in case (findTarget pos sq) of
                                Just tg ->
                                    -- prevent same square drop
                                    if tg.valid && tg.position /= sq.position
                                    -- complete move by drag
                                    then fullMove (sq => Square pos sq.piece True)
                                    else Nothing
                                Nothing -> Nothing
                        -- dead branch
                        Nothing -> Nothing 
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
                                    defaults
                                    |> validate s2
                                    |> addPiece s2.position s2.piece
                                    |> clear
                                Nothing -> defaults
                        Nothing -> 
                            -- invalid move, return piece back
                            -- using selection square
                            case player.select of
                                Just sq -> addPiece sq.position sq.piece board
                                Nothing -> defaults
        -- update history based on next move
        newHistory : History
        newHistory = 
            case nextMove of
                Just ((sq1, sq2) as move) ->
                    case sq2 of
                        Just s2 -> history ++ [move]
                        Nothing -> history
                Nothing -> history

    in Chess nextBoard playerMove newHistory ! []
                                           

startDrag : Mouse.Position -> Square -> Square
startDrag ps sq = 
    case sq.piece of
            Just p -> { sq | position = ps }
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
        Matrix.map (\{position,piece} -> Square position piece isValid) board

liftPiece : Square -> Board -> Board
liftPiece sq bd = 
    Matrix.update (toLocation sq.position) (\s -> { s | piece = Nothing }) bd

addPiece : Mouse.Position -> Maybe Piece -> Board -> Board
addPiece ps pc bd = 
    Matrix.update (toLocation ps) (\s -> 
        if s.valid 
        then { s | piece = pc, valid = True } 
        else s) bd

-----

validate : Square -> Board -> Board
validate sq bd =
    -- append input square as valid
    let validSquares = sq::(getValidSquares sq bd)
        checkMoves sq_ b = 
            Matrix.update (toLocation sq_.position) 
                (\{ position, piece, valid } ->
                    Square position piece True) b 
    in List.foldl checkMoves bd validSquares

getValidSquares : Square -> Board -> List Square
getValidSquares sq bd = (flip filterSameSquares) bd <| getPossible sq bd

filterSameSquares : List Square -> Board -> List Square
filterSameSquares squares bd =
    let filterSquare target =
            let square = Matrix.get (toLocation target.position) bd
            in case square of
                Just sq -> isSameColor sq target
                Nothing -> False
    in List.filter filterSquare squares

isSameColor : Square -> Square -> Bool
isSameColor s1 s2 = 
    let isWhite p =
            case p of
                White _ -> True
                Black _ -> False
        avoidWhite p =
            case p of
                White _ -> False
                Black _ -> True
        avoidBlack p =
            case p of
                White _ -> True
                Black _ -> False
        checkRule p1 p2 =
            if isWhite p1
            then avoidWhite p2
            else avoidBlack p2
        valid = Maybe.map2 checkRule s1.piece s2.piece 
    in case valid of
        Just v -> v
        Nothing -> True

