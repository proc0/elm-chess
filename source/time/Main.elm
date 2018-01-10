module Time.Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Mouse exposing (..)
import Json.Decode as Json exposing (..)
import Debug exposing (..)

import Data.Main exposing (..)
import Data.Game as Game exposing (..)
import Settings exposing (..)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SquareClicked position ->
            onSquareClicked (selectSquare position) model

onMouseUp : Attribute Msg
onMouseUp = on "mouseup" (Json.map SquareClicked Mouse.position)

onSquareClicked : Game.Position -> Model -> ( Model, Cmd Msg )
onSquareClicked position model =
    let nextModel = processMouseUp position model
    in ( nextModel, Cmd.none )

selectSquare : Mouse.Position -> Game.Position
selectSquare position =
    Game.Position (position.x // squareSize) (position.y // squareSize)

processMouseUp : Game.Position -> Model -> Model
processMouseUp position model = let l = log "p" position in model