module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Tuple exposing (..)
import Json.Decode as Json exposing (..)
import Mouse exposing (..)

import Game exposing (..)
import Settings exposing (..)
import Notation.FEN as FEN exposing (..)
--import Move as Move exposing (..)
--import SAN as SAN exposing (..)
--import Engine as Engine exposing (..)
--import Array exposing (..)
--import Debug exposing (..)
--import Task exposing (..)
--import Time exposing (..)

--type alias Point =
--    { x : String
--    , y : String
--    }

type alias Model =
    { game : Game.Model
    --, potentialMoves : List Game.Position
    --, selected : Maybe Chess.Position
    --, check : Bool
    --, checkMate : Bool
    --, moves : AllMoves
    --, computerThinking : Bool
    }

type Msg
    = SquareClicked Mouse.Position
    --| StartEngine
    --| ComputerMove (Maybe Move)

main : Program Never Model Msg
main = Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

fst = Tuple.first
snd = Tuple.second
(=>) = (,)

init : ( Model, Cmd Msg )
init = Model (FEN.toModel initialBoard) ! []
    --Model (FEN.toModel initialBoard) [] Nothing False False [] False ! []

view : Model -> Html Msg
view model =
    div []
        [ (boardWithPieces model)
        --, (moveHistory model)
        ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SquareClicked position ->
            onSquareClicked (selectSquare position) model

        --StartEngine ->
        --    { model | computerThinking = True } ! [ executeEngine model ]

        --ComputerMove maybeMove ->
        --    case maybeMove of
        --        Just ( srcPosition, destPosition ) ->
        --            processMove srcPosition destPosition model ! []

        --        Nothing ->
        --            { model | checkMate = True } ! []

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none
    --if ((not model.computerThinking && not model.game.whitesMove)) then
    --    Time.every (millisecond * 50) startEngine
    --else
    --    Sub.none       

onSquareClicked : Game.Position -> Model -> ( Model, Cmd Msg )
onSquareClicked position model =
    let nextModel = processMouseUp position model
    in ( nextModel, Cmd.none )

    --in case nextModel.game.whitesMove of
    --        False -> ( nextModel, Cmd.none )
    --        True  -> ( nextModel, Cmd.none )

selectSquare : Mouse.Position -> Game.Position
selectSquare position =
    Game.Position (position.x // squareSize) (position.y // squareSize)

processMouseUp : Game.Position -> Model -> Model
processMouseUp position model = model
    --let gameSquare = Move.getGameSquare position model.game.board
    --    potential = List.filter (\item -> item == position) model.potentialMoves
    --in case model.selected of
    --        Just selectedValue ->
    --            if position == selectedValue then
    --                clearSelected model |> clearMoves
    --            else if (List.length potential == 1) then
    --                processMove selectedValue position model
    --            else case gameSquare of
    --                    Just squareValue -> case squareValue of
    --                        Vacant -> model
    --                        Occupied player piece ->
    --                            if player == White then
    --                                model |> (setSelected position) |> updateMovesForSelected
    --                            else
    --                                model
    --                    Nothing -> model
    --        Nothing -> case gameSquare of
    --            Just squareValue ->
    --                case squareValue of
    --                    Vacant -> model
    --                    Occupied player piece ->
    --                        if (player == White) && model.game.whitesMove then
    --                            model |> (setSelected position) |> updateMovesForSelected
    --                        else if (player == Black) && (not model.game.whitesMove) then
    --                            model |> (setSelected position) |> updateMovesForSelected
    --                        else
    --                            model

    --            Nothing -> model
boardWithPieces : Model -> Html Msg 
boardWithPieces model =
    let chessboard = renderBlankBoard
                --++ (renderSelected model.selected)
                --++ (renderPotential model.potentialMoves)
                --++ (renderPieces model.game.board)
    in div [] chessboard

renderBlankBoard : List (Html Msg)
renderBlankBoard = List.map rank (List.range 0 7) |> List.concat

rank : Int -> List (Html Msg)
rank x = List.map (tile x) (List.range 0 7)

tile : Int -> Int -> Html Msg
tile xPos yPos =
    let xStr = (xPos * squareSize) |> px
        yStr = (yPos * squareSize) |> px
        backgroundColor = squareColor xPos yPos
    in div
        [ onMouseUp
        , style
            [ "backgroundColor" => backgroundColor
            , "width" => px squareSize
            , "height" => px squareSize
            , "position" => "absolute"
            , "left" => xStr
            , "top" => yStr
            ]
        ]
        []

onMouseUp : Attribute Msg
onMouseUp = on "mouseup" (Json.map SquareClicked Mouse.position)

squareColor : Int -> Int -> String
squareColor x y =
    let black = isBlack x y
    in if black then
            fst squareColors
        else
            snd squareColors

px : Int -> String
px value = (toString value) ++ "px"

isBlack : Int -> Int -> Bool
isBlack x y = (rem (x + y) 2) == 0