module Frame.Main exposing (..)

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

onMouseDown : Attribute Msg
onMouseDown =
  on "mousedown" (Json.map PieceLift Mouse.position)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ game, drag } as model) =
    let nextDrag = case msg of
                        PieceLift ps -> Just (Drag ps ps)
                        PieceDrag ps -> Maybe.map (\{start} -> Drag start ps) drag
                        PieceDrop _  -> Nothing             
    in (Model game nextDrag, Cmd.none)

            --PieceMove move ->
            --        { game =
            --           { board = updateBoard move game.board
            --           , select = let m2 = snd move 
            --                      in case m2 of
            --                              Occupied pos pc -> findSquare pos game.board
            --                              Vacant pos      -> findSquare pos game.board
            --           , history = game.history
            --           }
            --        , position = getPosition model
            --        , drag = Nothing     
            --        }

--toGamePosition : Mouse.Position -> Game.Position
--toGamePosition position =
--    Game.Position (position.x // squareSize) (position.y // squareSize)

--onSquareClicked : Game.Position -> Model -> ( Model, Cmd Msg )
--onSquareClicked position model =
--    let prevSelection = model.game.select
--        nextModel = processMouseUp position model
--    in case prevSelection of
--            Occupied _ _ -> update (PieceMove (prevSelection, nextModel.game.select)) model
--            Vacant _ -> ( nextModel, Cmd.none )

--processMouseUp : Game.Position -> Model -> Model
--processMouseUp position ({ game } as model) = 
--    { game = 
--        { board = game.board
--        , select = findSquare position game.board
--        , history = game.history
--        }
--    , position = getPosition model
--    , drag = Nothing  
--    }

--findSquare : Game.Position -> Game.Board -> Square
--findSquare pos board = 
--    let retrieve index items = 
--            case items of 
--                 Just list -> list 
--                    |> Array.fromList 
--                    |> Array.get index
--                 Nothing -> Nothing
--        getSelection b = 
--            retrieve pos.x <| 
--            retrieve pos.y <| Just b
--    in case getSelection board of
--            Just match -> match
--            Nothing    -> Vacant pos

--getPosition : Model -> Mouse.Position
--getPosition {game, position, drag} =
--  case drag of
--    Nothing -> position

--    Just {start,current} ->
--        let curPos = toPosition (current.x, current.y)
--            gamPos = toPosition ((position.x + curPos.x - start.x), (position.y + curPos.y - start.y))
--        in Mouse.Position gamPos.x gamPos.y

--updateBoard : Move -> Board -> Board
--updateBoard (m1, m2) board = 
--      let movingPc = case m1 of 
--                          Occupied ps pc -> Just pc
--                          Vacant ps -> Nothing
--      in case m2 of
--              Occupied ps pc -> case movingPc of
--                                     Just p -> List.map (updateRank p ps) board
--                                     Nothing -> board
--              Vacant ps -> board

--updateRank : Piece -> Game.Position -> Rank -> Rank
--updateRank pc ps rk = List.map (updateSquare pc ps) rk

--updateSquare : Piece -> Game.Position -> Square -> Square
--updateSquare pc pos sq = 
--      let isSq p = p.x == pos.x && p.y == pos.y
--      in case sq of 
--          Occupied ps pec -> if isSq ps then Occupied ps pc else Occupied ps pec
--          Vacant ps -> if isSq ps then Occupied ps pc else Vacant ps
