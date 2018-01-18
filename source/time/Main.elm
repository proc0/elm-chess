module Time.Main exposing (..)

import Array exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Mouse exposing (..)
import Json.Decode as Json exposing (..)
import Debug exposing (..)

import Data.Main exposing (..)
import Data.Game as Game exposing (..)
import Settings exposing (..)
import Toolkit exposing (..)

onMouseUp : Attribute Msg
onMouseUp = on "mouseup" (Json.map SquareClick Mouse.position)

onMouseDown : Attribute Msg
onMouseDown =
  on "mousedown" (Json.map DragStart Mouse.position)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SquareClick position ->
            onSquareClicked (selectSquare position) model
        DragStart xy ->
          (Model model.game model.position (Just (Drag xy xy)), Cmd.none)

        DragAt xy ->
          (Model model.game model.position (Maybe.map (\{start} -> Drag start xy) model.drag), Cmd.none)

        DragEnd _ ->
          (Model model.game (getPosition model) Nothing, Cmd.none)

        PieceMove move -> 
            let _ = log "move" move
                newModel = 
                    { game =
                       { board = updateBoard move model.game.board
                       , select = let m2 = snd move 
                                  in case m2 of
                                          Occupied pos pc -> findSquare pos model.game.board
                                          Vacant pos      -> findSquare pos model.game.board
                       , history = model.game.history
                       }
                    , position = getPosition model
                    , drag = Nothing     
                    }
            in (newModel, Cmd.none)

selectSquare : Mouse.Position -> Game.Position
selectSquare position =
    Game.Position (position.x // squareSize) (position.y // squareSize)

onSquareClicked : Game.Position -> Model -> ( Model, Cmd Msg )
onSquareClicked position model =
    let prevSelection = model.game.select
        nextModel = processMouseUp position model
    in case prevSelection of
            Occupied _ _ -> update (PieceMove (prevSelection, nextModel.game.select)) model
            Vacant _ -> ( nextModel, Cmd.none )

processMouseUp : Game.Position -> Model -> Model
processMouseUp position ({ game } as model) = 
    { game = 
        { board = game.board
        , select = findSquare position game.board
        , history = game.history
        }
    , position = getPosition model
    , drag = Nothing  
    }

findSquare : Game.Position -> Game.Board -> Square
findSquare pos board = 
    let retrieve index items = 
            case items of 
                 Just list -> list 
                    |> Array.fromList 
                    |> Array.get index
                 Nothing -> Nothing
        getSelection b = 
            retrieve pos.x <| 
            retrieve pos.y <| Just b
    in case getSelection board of
            Just match -> match
            Nothing    -> Vacant pos

getPosition : Model -> Mouse.Position
getPosition {game, position, drag} =
  case drag of
    Nothing ->
      position

    Just {start,current} ->
      Mouse.Position
        (position.x + current.x - start.x)
        (position.y + current.y - start.y)

updateBoard : Move -> Board -> Board
updateBoard (m1, m2) board = 
      let movingPc = case m1 of 
                          Occupied ps pc -> Just pc
                          Vacant ps -> Nothing
      in case m2 of
              Occupied ps pc -> case movingPc of
                                     Just p -> List.map (updateRank p ps) board
                                     Nothing -> board
              Vacant ps -> board

updateRank : Piece -> Game.Position -> Rank -> Rank
updateRank pc ps rk = List.map (updateSquare pc ps) rk

updateSquare : Piece -> Game.Position -> Square -> Square
updateSquare pc pos sq = 
      let isSq p = p.x == pos.x && p.y == pos.y
      in case sq of 
          Occupied ps pec -> if isSq ps then Occupied ps pc else Occupied ps pec
          Vacant ps -> if isSq ps then Occupied ps pc else Vacant ps
