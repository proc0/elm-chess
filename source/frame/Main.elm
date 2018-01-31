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
   let 
        selected : Maybe Square
        selected = 
            let target p = Matrix.get (toLocation <| getPosition p) board
            in case msg of
                Click xy -> target xy
                Drag xy  -> target xy
                Drop xy  -> target xy

        playerMove : Player
        playerMove = 
            case msg of 
                Click xy ->
                    case selected of
                        Just s  -> { player | select = selected, drag = Just (startDrag xy s) }
                        Nothing -> { player | select = Nothing, drag = Nothing }
                Drag xy -> 
                    case player.drag of
                        Just d -> { player | drag = Just { d | position = xy }}
                        Nothing -> player
                Drop xy -> { player | drag = Nothing }


        nextBoard : Board
        nextBoard = 
            let clear = toggleValid False
                defaults = clear board
                toValid = flip validate defaults
            in case msg of
                Drag _  -> board
                Click xy -> 
                    case selected of
                        Just sq -> 
                            case sq.piece of 
                                Just pc -> defaults |> validate sq |> liftPiece sq 
                                Nothing -> 
                                    case player.select of
                                        Just sq -> 
                                            let position = getPosition xy
                                                target = Matrix.get (toLocation position) (toValid sq) 
                                            in case target of
                                                Just tg ->
                                                    if tg.valid && tg.position /= sq.position
                                                    then toValid sq |> liftPiece sq |> addPiece position sq.piece |> clear
                                                    else defaults
                                                Nothing -> defaults
                                        Nothing -> defaults 
                        Nothing -> defaults

                Drop xy ->
                    case player.select of
                        Just sq -> 
                            let position = getPosition xy
                                target = Matrix.get (toLocation position) (toValid sq) 
                            in case target of
                                Just tg ->
                                    if tg.valid && tg.position /= sq.position
                                    then addPiece position sq.piece (toValid sq) |> clear
                                    else addPiece sq.position sq.piece board
                                Nothing -> defaults
                        Nothing -> defaults 

    in Chess nextBoard playerMove history ! []
                                           

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

moveSquare : Square -> Square -> Square
moveSquare current target = 
        if target.position == current.position && target.valid 
        then { current | valid = True }
        else target

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

